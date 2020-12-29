{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell #-}

module Main (main) where

import Prelude hiding (log)

import Control.Monad (unless, forever, void)
import Control.Concurrent
import Control.Exception (catch, IOException)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import System.IO
import System.Directory
import Text.Read (readMaybe)
import System.Random (randomRIO)
import qualified Data.ByteString.Lazy as LS hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as LS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.List (splitAt, sort)
--import Data.Text hiding (length, splitOn, map, filter)
import Data.Text (Text, pack, unpack)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Network.Run.TCP
import Network.Socket
import qualified Network.WebSockets as WS
import Network.Socket.ByteString (recv)
import Data.Aeson
import Data.Aeson.TH

type PlayerId = Int
type PlayerName = Text

data Clue = Clue { clwd :: Text, shown :: Bool } deriving (Show, Eq)

type Clues = Map PlayerId Clue
type ShownClues = Map PlayerId Text

--type WordSource = String
--
type Colour = Int

data MsgIn = Join { jname :: Text }
           | StatusReq
           | NextTurn
           | NextPhase GamePhase
           | SubmitClue Text
           | SubmitGuess Text
           | ClueVis PlayerId Bool
           | Ready
           | Quit
           | RemovePlayer PlayerId
           | SetSource WordSource
           | SetColourMI Colour
           | ReqWordFiles
           | AskWord
           | ChatMI Text
             deriving (Show, Eq)

data FileWgt = FileWgt { filename :: String, weight :: Int } deriving (Show, Eq)

fileWgt = FileWgt

type WordSource = [FileWgt]

data MsgOut = ShowWord
            | StatusUpd { self :: Player
                        , all :: [PubStatus]
                        , suphase :: GamePhase
                        , suguesser :: Maybe Text
                        , private :: PersStatus
                        , sourceOfWord :: Maybe Text
                        , susettings :: GameSettings }
            | ErrorMsg Text
            | IntlPlayerData PlayerData
            | Removed
            | GiveWord Text
            | SetColourMO PlayerName Colour
            | SendWordSource WordSource
            | SendColours (Map PlayerName Colour)
            | ChatMO { from :: Text, content :: Text}
              deriving (Show, Eq)

instance FromJSON MsgIn where
    parseJSON (Object msg) = do
        tp <- msg .: "msgtype"
        case (tp :: Text) of
            "join" -> Join <$> msg .: "name"
            "statusreq" -> return StatusReq
            "nextturn" -> return NextTurn
            "nextphase" -> NextPhase <$> msg .: "currphase"
            "sendclue" -> SubmitClue <$> msg .: "clue"
            "sendguess" -> SubmitGuess <$> msg .: "guess"
            "cluevis" -> ClueVis <$> msg .: "playerid" <*> msg .: "visible"
            "ready" -> return Ready
            "quit" -> return Quit
            "getword" -> return AskWord
            "reqwordfiles" -> return ReqWordFiles
            "setsource" -> SetSource <$> msg .: "source"
            "setcolour" -> SetColourMI <$> msg .: "colour"
            "remplr" -> RemovePlayer <$> msg .: "id"
            "chat" -> ChatMI <$> msg .: "text"

t :: Text -> Text
t = id

instance ToJSON MsgOut where
    toJSON ShowWord = object []
    toJSON (StatusUpd slf pls phs gsr pstat src settings) = object ["msgtype" .= t "status",
        "self" .= slf, "players" .= pls, "guesser" .= gsr, "phase" .= phs, "pers_status" .= pstat, "source" .= src, "settings" .= settings ]
    toJSON Removed = object ["msgtype" .= t "removed"]
    toJSON (ErrorMsg e) = object ["msgtype" .= t "error", "msg" .= e]
    toJSON (GiveWord w) = object ["msgtype" .= t "giveword", "word" .= w]
    toJSON (ChatMO p c) = object ["msgtype".= t "chat", "text" .= c, "from" .= p]
    toJSON (SetColourMO p c) = object ["msgtype".= t "setcolour", "colour" .= c, "player" .= p]
    toJSON (SendWordSource fs) = object ["msgtype".= t "wordsource", "source" .= fs]
    toJSON (SendColours cls) = object ["msgtype".= t "allcolours", "colours" .= cls]

instance ToJSON Clue where
    toJSON (Clue w s) = object ["word".= w, "shown" .= s]

instance ToJSON Player where
    toJSON (Player i n g r) = object ["id".= i, "name" .= n, "is_gm" .= g, "ready" .= r]

addTo (Object o) l = Object . HM.union o . (\(Object x) -> x) $ object l

instance ToJSON PubStatus where
    toJSON (Guesser p) = addTo (toJSON p) ["role" .= t "guess"]
    toJSON (ClueMaker p) = addTo (toJSON p) ["role" .= t "clue"]

instance ToJSON PersStatus where
    toJSON (Guessing sc) = object ["role" .= t "guess", "clues" .= sc ]
    toJSON (ClueMaking w m c) =  object ["role" .= t "clue", "word" .= w, "myclue" .= m, "clues" .= c]
    toJSON (Unspecified w c g) =  object ["word" .= w, "clues" .= c, "guess" .= g]

instance ToJSON GamePhase where
    toJSON = String . pack . show

instance FromJSON GamePhase where
    parseJSON (String s) = case readMaybe (unpack s) of
        (Just p) -> return p
        Nothing -> fail "not a phase"
    parseJSON _ = fail "not a string"



data PubStatus = Guesser { psplayer :: Player }
                | ClueMaker { psplayer :: Player}
                  deriving (Show, Eq)

data PersStatus = Guessing (Maybe ShownClues)
                | ClueMaking { psword :: Text, psmyclue :: Maybe Text, psclues :: Clues }
                | Unspecified { psword :: Text, psclues :: Clues, psguess :: Text }
                  deriving (Show, Eq)

data PlayerData = PD {player :: Player, messages :: Chan MsgOut } --deriving (Show)

instance Show PlayerData where
    show p = "PlayerData { " ++ show (player p) ++ "}"

instance Eq PlayerData where 
    p == q = player p == player q

instance Ord PlayerData where 
    p <= q = player p <= player q

data Player = Player { pid:: PlayerId, name :: Text, gm :: Bool, ready :: Bool} deriving (Show)

instance Eq Player where 
    p == q = pid p == pid q

instance Ord Player where 
    p <= q = pid p <= pid q

data PlayerMsg = NewPlayer Text (Chan MsgOut)
               | PlayerMsg PlayerData MsgIn

data GameState = GS { players :: [PlayerData]
                    , whoseTurn :: PlayerId
                    , phase :: GamePhase
                    , word :: Text
                    , guess :: Text
                    , wordSource :: WordSource
                    , thisSource :: Text
                    , clues :: Map PlayerId Clue
                    , settings :: GameSettings
                    , colours :: Map PlayerName Colour} --deriving (Show)

data GameSettings = GameSettings { showSource :: Bool } deriving (Show, Eq)

data GamePhase = Prelim
               | MakeClues
               | InspectClues
               | MakeGuess
               | Complete deriving (Show, Eq, Read)

type Game a = ReaderT (Chan String) (StateT GameState IO) a
type Log = Chan String

type Logger a = ReaderT Log IO a

log :: (MonadIO m, MonadReader Log m) => String -> m ()
log s = do
    c <- ask
    liftIO $ writeChan c s 

updateWordSource_ :: WordSource -> IO WordSource
updateWordSource_ ws = do
    files <- getWordFiles
    return $ (ws ++) . map (flip fileWgt 0) $ filter (\f -> not . or $ map ((== f) . filename) ws) files

updateWordSource :: Game ()
updateWordSource = do
    state <- get
    ws <- liftIO $ updateWordSource_ (wordSource state)
    put state{ wordSource = ws }


newGameState :: IO GameState
newGameState = do
    src <- updateWordSource_ [fileWgt "words.txt" 1]
    let settings = GameSettings True
    return $ GS { players = []
                , whoseTurn = (-1)
                , phase = Prelim
                , word = "", guess = ""
                , wordSource =  src
                , clues = M.empty
                , thisSource = ""
                , settings = GameSettings True
                , colours = M.empty }

runGame :: Game a -> GameState -> Log -> IO ()
runGame g gs l = do
    runStateT (runReaderT  g l) gs
    return ()

runLogger l x = runReaderT x l

getPlayerId :: Text -> Game Int
getPlayerId n = do
    let idif p = if name p == n then Just (pid p) else Nothing
    s <- get
    let prevId = filter ((==n) . name . player)  . players $ s
    case prevId of
        [] -> return $ if null (players s) then 0 else (+1) . maximum . map (pid . player) . players $ s
        pd:[] -> return . pid $ player pd
        _ -> do
            log $ "multiple players named " ++ unpack n
            return (-1)


send :: PlayerData -> MsgOut -> Game ()
send p m = liftIO $ writeChan (messages p) m

randomElt l = (l !!) <$> randomRIO (0, length l -1)

getBoard :: Game [Text]
getBoard = (take 25 . (foldl remDups [])) <$> (sequence $ repeat getWord)
    where
--      remDups :: [Text] 
      remDups ys x = if x `elem` ys
                        then ys
                        else ys ++ [x]

getWord :: Game Text
getWord = do
    src <- wordSource <$> get
    srcFile <- liftIO $ randomElt . foldl (++) [] $ map (\fw  -> replicate (weight fw) (filename fw)) src
    modify $ \s -> s {thisSource = pack srcFile}
    liftIO $ do
        file <-  openFile ("words/" ++ srcFile) ReadMode
        words <- filter (not . null) . splitOn "\n" <$> hGetContents file
        pack <$> randomElt words

allCluesSubmitted :: Game Bool
allCluesSubmitted = do
    s <- get
    return $ foldl (&&) True . map (`M.member` clues s) . filter (/= whoseTurn s) . map (pid . player) $ players s

replaceIf f x = map (\y-> if f y then x else y)

applyIf f g = map (\y-> if f y then g y else y)

replaceOrInsert :: Eq a => a -> a -> [a] -> [a]
replaceOrInsert x y l = if x `elem` l then replaceIf (==x) y l else l ++ [y]

nextPlayer = do
    s <- get
    let init = if phase s == Prelim then 0 else whoseTurn s + 1
        ids = map (pid . player) $ players s
        maybeSet i =
            if i `elem` ids
            then put s {whoseTurn = i}
            else maybeSet $ if i >= maximum ids
                            then 0 else i + 1
    maybeSet init
            
nextTurn = do
    setAllReady False
    nextPlayer
    wd <- getWord
    modify $ \s -> s { word = wd, phase = MakeClues , clues = M.empty}

gameserver :: Chan PlayerMsg -> Game ()
gameserver chan = do 
    pmsg <- liftIO $ readChan chan
    handlePMsg pmsg
    gameserver chan
  where
    handlePMsg :: PlayerMsg -> Game ()
    handlePMsg (NewPlayer n c) = do
        i <- getPlayerId n
        let pd = PD (Player i n True False) c
        modify $ \s -> s { players = replaceOrInsert pd pd (players s) }
        liftIO $ writeChan c $ IntlPlayerData pd
        s <- get
        liftIO $ writeChan c . SendColours $ colours s
        sendWordSource pd
        broadcastState
    handlePMsg (PlayerMsg p m) = log (show m) >> handleMsg p m
    handleMsg :: PlayerData -> MsgIn -> Game ()
    handleMsg plr (NextPhase p) = do
        curr <- phase <$> get 
        when (curr == p) nextPhase
        broadcastState
    handleMsg plr AskWord = do
        word <- getWord
        send plr $ GiveWord word
    handleMsg plr NextTurn = do
        nextTurn
        broadcastState
    handleMsg plr (SubmitClue c) = do
        s <- get
        if phase s == MakeClues
        then if pid (player plr) /= whoseTurn s
          then do
            put s { clues = M.insert (pid $ player plr) (Clue c True) $ clues s }
            setReady (player plr) True
            allclues <- allCluesSubmitted
            when allclues nextPhase
            broadcastState
          else send plr $ ErrorMsg "not clue maker"
        else send plr $ ErrorMsg "not clue time"
    handleMsg plr (SubmitGuess g) = do
        s <- get
        if phase s == MakeGuess
        then if pid (player plr) == whoseTurn s
          then do
            put s { guess = g, phase = Complete }
            broadcastState
          else send plr $ ErrorMsg "not guesser"
        else send plr $ ErrorMsg "not guess time"
    handleMsg plr (ClueVis p v) = do
        modify $ \s -> s {clues = M.adjust (\c -> c {shown = v}) p $ clues s }
        broadcastState
    handleMsg plr Ready = do
        setReady (player plr) True
        s <- get
        case phase s of
            InspectClues -> when allReady nextPhase
                where allReady = (and . map ready . filter ((/= whoseTurn s) . pid) . map player $ players s)
            Complete -> when allReady nextPhase
                where allReady = (and . map (ready . player) $ players s)
            _ -> return ()
        broadcastState
    handleMsg plr (ChatMI c) = do
        sendAll $ ChatMO (name $ player plr) c
    handleMsg plr ReqWordFiles = sendWordSource plr
    handleMsg plr (SetSource src) = do
        valid <- and <$> mapM (validateFW plr) src
        when valid $
          if (and $ map ((==0) . weight) src)
          then send plr $ ErrorMsg "empty source!"
          else do
            modify (\s -> s{ wordSource = src })
            sendallWordSource
    handleMsg plr (SetColourMI c) = do
        sendAll $ SetColourMO (name $ player plr) c
        modify $ \s -> s{ colours = M.insert (name $ player plr) c $ colours s }
    handleMsg plr Quit = do
        removePlayer plr
        broadcastState
    handleMsg plr (RemovePlayer i)= do
        mp <- playerById i
        case mp of
          (Just plr) -> do
            removePlayer plr
            broadcastState
          Nothing -> send plr $ ErrorMsg "no such player"

validateFW plr (FileWgt _ w)
    | w < 0 = send plr (ErrorMsg "negative weight") >> return False
    | w > 100 = send plr (ErrorMsg "weight too large") >> return False
    | otherwise = return True

getWordFiles = listDirectory "words"

sendallWordSource = mapM_ sendWordSource =<< players <$> get

sendWordSource plr = do
    updateWordSource
    liftIO . writeChan (messages plr) . SendWordSource =<< wordSource <$> get

nextPhase :: Game ()
nextPhase = do
    setAllReady False
    s <- get
    case (phase s) of
      Prelim -> nextTurn
      MakeClues -> modify (\s -> s { phase = InspectClues }) 
      InspectClues -> do
        put s {phase = MakeGuess}
      MakeGuess -> put s {phase = Complete }
      Complete -> nextTurn

sendAll msg = do
    chans <- map messages . players <$> get
    liftIO $ mapM_ (flip writeChan msg) chans

removePlayer plr = do
    s <- get
    put s {players = filter (/= plr) $ players s }
    when (whoseTurn s == pid (player plr)) nextTurn -- TODO
    liftIO $ writeChan (messages plr) Removed


setAllReady :: Bool -> Game ()
setAllReady val = modify $ \s -> s {players = map (\pd ->  pd { player = (player pd) {ready = val}}) $ players s}

setReady :: Player -> Bool -> Game ()
setReady p val = modify $ \s -> s {players = applyIf ((== p) . player) (\pd ->  pd { player = (player pd) {ready = val}}) $ players s}

otherPlayers :: PlayerData -> Game [PlayerData]
otherPlayers p = do
    sort . players <$> get

playerById :: PlayerId -> Game (Maybe PlayerData)
playerById i = getFirst . filter ((== i) . pid . player) . players <$> get
  where
    getFirst (x:xs) = Just x
    getFirst [] = Nothing

--playersTurn :: Game Player
--playersTurn = do
--    s <- get
--    player <$> playerById (whoseTurn s)

persStatus :: PlayerData -> Game PersStatus
persStatus pd = do
    s <- get
    let p = player pd
    if phase s `elem` [Prelim, Complete]
    then return $ Unspecified (word s) (clues s) (guess s)
    else return $ if whoseTurn s == pid p
        then let showClues = if phase s == MakeGuess
                             then Just (M.map clwd . M.filter shown $ clues s)
                             else Nothing
             in Guessing showClues
        else let cls = if phase s /= MakeClues
                       then clues s
                       else M.empty
             in ClueMaking (word s) (fmap clwd . M.lookup (pid p) $ clues s) cls

pubStatus :: PlayerData -> Game PubStatus
pubStatus pd = do
    s <- get
    let p = player pd
    return $ if whoseTurn s == pid p
    then Guesser p
    else ClueMaker p

broadcastState :: Game ()
broadcastState = do
    s <- get
    mapM_ sendState $ players s

sendState pd = do
    s <- get
    pubstats <- mapM pubStatus $ players s
    ps <- persStatus pd
    guesser <- playerById $ whoseTurn s
    liftIO $ writeChan (messages pd) $ StatusUpd (player pd) pubstats (phase s) (name . player <$> guesser) ps (Just $ thisSource s) (settings s)

wsServer :: Log -> WS.PendingConnection -> IO ()
wsServer l p = do
    let log = writeChan l
    log "wsServer"
    conn <- WS.acceptRequest p
    log "request accepted"
    let handle err = log $ "exception: " ++ show (err :: IOException)
    flip catch (handle) $ runTCPClient "127.0.0.1" "3000" $ \sock -> do
        log "started tcp client"
        hdl <- socketToHandle sock ReadWriteMode
        forkIO . forever $ do
            bs <- LS.fromStrict <$> BS.hGetLine hdl
--            log $ "passing outgoing message: " ++ show bs
            WS.sendTextData conn $ bs <> "\n"
        forever $ do
            (WS.Text bs _) <- WS.receiveDataMessage conn
--            log $ "passing incoming message: " ++ show bs
            LS.hPutStr hdl bs 
    

readMsg :: Handle -> Logger MsgIn
readMsg hdl = do
    jsn <- liftIO $ LS.fromStrict <$> BS.hGetLine hdl
    --log $ LS.unpack jsn 
    case eitherDecode jsn of
      (Right msg) -> return msg
      (Left str) -> log str >> log (show jsn) >> readMsg hdl

writeMsg :: MsgOut -> Handle -> Logger ()
writeMsg m h = do
    let msgstr = encode m
    log $ "to send: " ++ show m
--    log . LS.unpack $ "\nsending '" <> msgstr <> "'\n"
    liftIO $ LS.hPutStrLn h msgstr

receive :: Chan PlayerMsg -> Log-> Socket -> IO ()
receive mainChan l sock = do
    hdl <- socketToHandle sock ReadWriteMode
    jmesg <- runLogger l $ readMsg hdl
    case jmesg of
      (Join nm) -> do
        ochan <- newChan
        writeChan mainChan $ NewPlayer nm ochan
        (IntlPlayerData player) <- readChan ochan
        tid <- forkIO $ recvMsgs player mainChan hdl
        sendMsgs ochan hdl tid
  where
    recvMsgs :: PlayerData -> Chan PlayerMsg -> Handle -> IO ()
    recvMsgs pid ch hdl = do
        msg <- runLogger l $ readMsg hdl
        writeChan ch $ PlayerMsg pid msg
        unless (msg == Quit) $ recvMsgs pid ch hdl
--    sendMsgs :: Chan MsgOut -> Handle -> IO ()
    sendMsgs ch hdl recvr = do
        msg <- readChan ch
        --writeChan l $ "sendMsgs: " ++ show msg
        runLogger l $ writeMsg msg hdl
        if msg == Removed
          then do
            writeChan l "killing reciever"
            killThread recvr
            writeChan l "closing socket"
            close' sock
            writeChan l "closed socket#"
            hClose hdl
            writeChan l "closed socket"
          else do
            sendMsgs ch hdl recvr

main :: IO ()
main = do
    mchan <- newChan
    gs <- newGameState
    pchan <- newChan
    forkIO $ printer pchan 
    forkIO $ runGame (gameserver mchan) gs pchan
    writeChan pchan "starting websock server"
    forkIO $ WS.runServer "0.0.0.0" 3001 $ wsServer pchan
    writeChan pchan "starting TCP server"
    runTCPServer Nothing "3000" $ receive mchan pchan
  where
    printer c = do
        s <- readChan c
        putStrLn s
        printer c
--  where
--    talk s = do
--        msg <- recv s 1024
--        unless (S.null msg) $ do
--          sendAll s msg
--          talk s

$(deriveJSON defaultOptions ''FileWgt)
$(deriveJSON defaultOptions ''GameSettings)
