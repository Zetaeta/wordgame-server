{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell #-}

module Main (main) where

import Prelude hiding (log)
import Game
import Serialize
import Codenames

import Control.Monad (unless, forever, void)
import Control.Concurrent
import Control.Exception (catch, IOException)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import System.IO
import System.Directory
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as LS hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as LS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
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
import qualified Data.HashMap.Strict as HM



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



getBoard :: Game [Text]
getBoard = (take 25 . (foldl remDups [])) <$> (sequence $ repeat getWord)
    where
--      remDups :: [Text] 
      remDups ys x = if x `elem` ys
                        then ys
                        else ys ++ [x]


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

newTurn = do
    setAllReady False
    wd <- getWord
    modify $ \s -> s { word = wd, phase = MakeClues , clues = M.empty}

gameserver :: Chan (PlayerData, CNMsg) -> Chan PlayerMsg -> Game ()
gameserver codechan chan = do 
    pmsg <- liftIO $ readChan chan
    handlePMsg pmsg
    gameserver codechan chan
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
    handleMsg plr NewWord = do
        newTurn
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
    handleMsg plr (AskCard s) = send plr $ GiveCard (newCard s)
    handleMsg plr (NewBoard bid) = do
        board <- newBoard bid
        liftIO $ writeChan codechan (plr, NewB board)
    handleMsg plr rest = liftIO $ writeChan codechan (plr, OthrM rest)

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
    log "writeMsg"
    let msgstr = encode m
    log $ "to send: " ++ show m
--    log . LS.unpack $ "\nsending '" <> msgstr <> "'\n"
    liftIO $ LS.hPutStrLn h msgstr
    log  "sent!" 

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
    codechan <- newChan
    forkIO $ runBoards (boardHandler codechan) pchan
    forkIO $ runGame (gameserver codechan mchan) gs pchan
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

