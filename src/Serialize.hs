{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell, FlexibleInstances #-}

module Serialize where

import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import Game

$(deriveJSON defaultOptions ''FileWgt)
$(deriveJSON defaultOptions ''GameSettings)
-- $(deriveJSON defaultOptions ''Team)
$(deriveJSON defaultOptions ''BoardUpdate)
$(deriveJSON defaultOptions ''CodeBoard)
$(deriveJSON defaultOptions ''Card)

pair a b = (a,b)


instance {-# OVERLAPPING #-} FromJSON BoardWord where
    parseJSON (Object obj) = pair <$> obj .: "w" <*> obj .: "t"


instance {-# OVERLAPPING #-} ToJSON BoardWord where
    toJSON (w,t) = object[ "w" .= w, "t" .= t]

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
            "newword" -> return NewWord
            "getboards" -> return AskBoards
            "getboard" -> GetBoard <$> msg .: "board_id"
            "newboard" -> NewBoard <$> msg .: "board_id"
            "getcard" -> AskCard <$> msg .: "seed"
            "delboard" -> DelBoard <$> msg .: "board_id"
            "leaveboard" -> LeaveBoard <$> msg .: "board_id"
            "changeboard" -> ChangeBoard <$> msg .: "board_id" <*> msg .: "change"
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
    toJSON (GiveCard c) = object ["msgtype" .= t "givecard", "card" .= c]
    toJSON (GiveBoard bd) = object ["msgtype" .= t "giveboard", "board" .= bd]
    toJSON (SendBoards bs) = object ["msgtype" .= t "sendboards", "boards" .= bs]
    toJSON (UpdateBoard bu) = object ["msgtype" .= t "updateboard", "update" .= bu]
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

