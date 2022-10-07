{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Game where

import Prelude hiding (log)
import Data.Text (Text, pack, unpack)
import Data.Map (Map)
import Data.List.Split (splitOn, chunksOf)
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import System.Random (randomRIO,randomIO)
import System.IO

type Log = Chan String

type Logger a = ReaderT Log IO a

type PlayerId = Int
type PlayerName = Text

data Clue = Clue { clwd :: Text, shown :: Bool } deriving (Show, Eq)

type Clues = Map PlayerId Clue
type ShownClues = Map PlayerId Text

--type WordSource = String
--
type Colour = Int


--data Team = Default | Neutral | Red | Blue | Assassin deriving (Show, Eq)
type Team = Int
defaultTeam = 0

type BoardWord = (Text, Team)

type BoardContents = [[BoardWord]]

data CodeBoard = CodeBoard { bid :: BoardID, contents :: BoardContents } deriving (Show, Eq)

type BoardID = String

data BoardIDReq = NB BoardID | BID BoardID deriving (Show, Eq)

data Card = Card { seed :: Text, firstt :: Int, cardclrs :: [[Team]], hash :: Int } deriving (Eq, Show)

data CNMsg = NewB CodeBoard | OthrM MsgIn 

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
           | NewWord
           | AskCard Text
           | AskBoards
           | DelBoard BoardID
           | LeaveBoard BoardID
           | GetBoard BoardID
           | NewBoard BoardID
           | ChangeBoard BoardID BoardUpdate
           | ChatMI Text
             deriving (Show, Eq)

data FileWgt = FileWgt { filename :: String, weight :: Int } deriving (Show, Eq)

fileWgt = FileWgt

type WordSource = [FileWgt]

data BoardUpdate = BoardUpdate { pos :: (Int, Int), wdclr :: Team } deriving (Show, Eq)

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
            | GiveBoard CodeBoard
            | GiveCard Card
            | UpdateBoard BoardUpdate
            | SendBoards [BoardID]
            | SetColourMO PlayerName Colour
            | SendWordSource WordSource
            | SendColours (Map PlayerName Colour)
            | ChatMO { from :: Text, content :: Text}
              deriving (Show, Eq)




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

type Metadata = [Player]

data GameM g a = GameM  {runGameM :: ReaderT (Chan String) (StateT (Metadata, g) IO) a}

instance Functor (GameM g) where
    fmap f (GameM r) = GameM $ fmap f r

instance Applicative (GameM g) where
    fs <*> xs = do
        f <- fs
        x <- xs
        pure (f x)
    pure x = GameM $ pure x

instance Monad (GameM g) where
    (GameM r) >>= f = GameM $ r >>=(runGameM . f)

instance MonadReader Log (GameM g) where
    ask = GameM ask
    local f (GameM g) = GameM $ local f g

instance MonadState g (GameM g) where
    get = GameM $ snd <$> get
    put s = GameM $ modify (\(m,g)-> (m,s)) 
instance MonadIO (GameM g) where
    liftIO a = GameM $ liftIO a

getWord :: Game Text
getWord = do
    src <- wordSource <$> get
    srcFile <- liftIO $ randomElt . foldl (++) [] $ map (\fw  -> replicate (weight fw) (filename fw)) src
    modify $ \s -> s {thisSource = pack srcFile}
    liftIO $ do
        file <-  openFile ("words/" ++ srcFile) ReadMode
        words <- filter (not . null) . splitOn "\n" <$> hGetContents file
        pack <$> randomElt words

getWithoutDups :: (Monad m, Eq a) => Int -> m a -> m [a]
getWithoutDups 0 _ = return []
getWithoutDups 1 s = return <$> s
getWithoutDups n s = do
    rest <- getWithoutDups (n-1) s
    nth <- getIf (not . (`elem` rest)) s
    return $ nth : rest
  where 
    getIf f s = do
        x <- s
        if f x then return x else getIf f s


getBoard :: Game BoardContents
getBoard = (chunksOf 5) . map (\x -> (x, defaultTeam)) <$> getWithoutDups 25 getWord

randID :: Game BoardID
randID = show . (abs :: Int -> Int) <$> liftIO randomIO

newBoard :: BoardID -> Game CodeBoard
newBoard id = do
    board <- getBoard
    id' <- case id of
      "" -> randID
      x -> return x
    log $ "new board: " ++ show board
    return $ CodeBoard id' board 

randomElt l = (l !!) <$> randomRIO (0, length l -1)

send :: (MonadIO m) => PlayerData -> MsgOut -> m ()
send p m = liftIO $ writeChan (messages p) m

log :: (MonadIO m, MonadReader Log m) => String -> m ()
log s = do
    c <- ask
    liftIO $ writeChan c s 

