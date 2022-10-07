
module Codenames where

import Prelude hiding (log)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Concurrent
import System.Random.Shuffle
import System.Random
import Data.Text (Text, unpack)
import Data.Map (Map, (!?), (!))
import Data.List.Index (updateAt)
import Data.List.Split (chunksOf)
import Data.Hashable (hash)
import qualified Data.Map as M
import qualified Data.Text as T

import Game hiding (hash)

type Boards = Map BoardID BoardData

data BoardData = BoardData { board :: CodeBoard, bplayers :: [PlayerData] } deriving (Show, Eq)

newCard :: Text -> Card
newCard seed = Card seed' first clrs h
  where
    seed' = T.toLower $ T.strip seed 
    s = mkStdGen $ hash $ T.unpack seed' :: StdGen
    (red, s') = random s
    first = team red
      where team True = 1
            team False = 3
    (h, s'') = randomR (0,9) s'
    clrs = chunksOf 5 $ shuffle' start 25 s''
    start = [1,1,1,1,1, 1,1,1,first,3, 3,3,3,3,3, 3,3,2,2,2, 2,2,2,2,4]

runBoards :: GameM Boards a -> Log  -> IO a
runBoards (GameM g) l = fst <$> runStateT (runReaderT g l) ([], M.empty)

modBoard (BoardUpdate (x,y) t) (CodeBoard i wds) = CodeBoard i $ updateAt x (Just . updateAt y (\(w,_)-> Just (w,t))) wds

broadcastBoard (BoardData board pls) = mapM_ (sendBoard board) pls 

sendBoard :: CodeBoard -> PlayerData -> GameM Boards ()
sendBoard b p = log ("sending board to " ++ unpack (name (player p))) >> (send p $ GiveBoard b) >> log "sent board"

boardHandler :: Chan (PlayerData, CNMsg) -> GameM Boards ()
boardHandler chan = do
    (plr, msg) <- liftIO $ readChan chan
    handleCMsg plr msg
    boardHandler chan
  where
    handleCMsg plr (NewB b) = do
        modify (M.insert (bid b) (BoardData b [plr]))
        sendBoard b plr
    handleCMsg plr (OthrM m) = handleMsg plr m
    handleMsg :: PlayerData -> MsgIn -> GameM Boards ()
    handleMsg plr AskBoards = do
        boards <- M.keys <$> get
        send plr $ SendBoards boards
    handleMsg plr (DelBoard  id) = do
        modify $ M.delete id
    handleMsg plr (LeaveBoard  id) = do
        boards <- get
        case (boards !? id) of
          Just b -> modify $ M.adjust (\(BoardData b ps) -> BoardData b $ filter (/= plr) ps) id
    handleMsg plr (GetBoard  id) = do
        boards <- get
        case (boards !? id) of
          Just b -> do
            send plr $ GiveBoard $ board b
            modify $ M.adjust (\(BoardData b ps) -> BoardData b (if elem plr ps then ps else plr:ps)) id
    handleMsg plr (ChangeBoard id chg) = do
        boards <- get
        let b = boards ! id
            nb = b {board = modBoard chg (board b)}
        put $ M.insert id nb boards
        broadcastBoard nb
    handleMsg plr msg = return () 
