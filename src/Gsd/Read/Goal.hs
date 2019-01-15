{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Gsd.Read.Goal where

import Data.Aeson
import Gsd.Write.Core
import Data.Text
import GHC.Generics

type GoalDescription = Text

data GoalStatus = Created | InProgress | Paused | Accomplished | GivenUp deriving (Show , Eq , Generic )

data Goal = Goal {workspaceId ::WorkspaceId , goalId :: GoalId , description :: GoalDescription , status :: GoalStatus} deriving (Show , Eq , Generic )


getNextStatusAvailable :: GoalStatus -> [GoalStatus]
getNextStatusAvailable currentStatus = case currentStatus of
    Created  -> [InProgress,Paused,Accomplished,GivenUp]
    InProgress  -> [Paused,Accomplished,GivenUp]
    Paused ->  [InProgress,Accomplished,GivenUp]
    Accomplished -> []
    GivenUp -> []

instance ToJSON Goal where
  toJSON (Goal {workspaceId, goalId ,description,status } ) = object [
            "workspaceId" .= workspaceId,
            "goalId" .= goalId,
            "description" .= description,
            "status" .= status]

instance FromJSON Goal  where

    parseJSON (Object jsonObject) = Goal <$> jsonObject .: "workspaceId" <*>  jsonObject .: "goalId" <*>  jsonObject .: "description" <*>  jsonObject .: "status"
    parseJSON _ =  error $ "Json format not expected"


instance ToJSON GoalStatus where
    toJSON (Created) = object ["value" .= String "created"]
    toJSON (InProgress) = object ["value" .= String "inProgress"]
    toJSON (Paused) = object ["value" .= String "paused"]
    toJSON (Accomplished) = object ["value" .= String "accomplished"]
    toJSON (GivenUp) = object ["value" .= String "givenUp"]


instance FromJSON GoalStatus  where

  parseJSON (Object o) = do
     value <- o .: "value"
     case value of
          String status | (unpack status) == "created" -> return Created
          String status | (unpack status) == "inProgress" -> return InProgress
          String status | (unpack status) == "paused" -> return Paused
          String status | (unpack status) == "accomplished" -> return Accomplished
          String status | (unpack status) == "givenUp" -> return GivenUp
          _ -> error $ "Json format not expected"
  parseJSON _ =  error $ "Json format not expected"