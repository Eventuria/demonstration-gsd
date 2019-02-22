{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Eventuria.GSD.Read.Model.Goal where

import Data.Aeson
import Eventuria.GSD.Write.Model.Core
import Data.Text
import GHC.Generics
import Eventuria.GSD.Read.Model.ActionStats

type GoalDescription = Text

data GoalStatus = Created
                | InProgress
                | Paused
                | Accomplished
                | GivenUp deriving (Show , Eq , Generic )

data Goal = Goal {workspaceId :: WorkspaceId ,
                  goalId :: GoalId ,
                  description :: GoalDescription ,
                  status :: GoalStatus,
                  actionStats :: ActionStats} deriving (Show , Eq , Generic )


getNextStatusAvailable :: GoalStatus -> [GoalStatus]
getNextStatusAvailable currentStatus = case currentStatus of
    Created  -> [InProgress,Accomplished,GivenUp]
    InProgress  -> [Paused,Accomplished,GivenUp]
    Paused ->  [InProgress,Accomplished,GivenUp]
    Accomplished -> []
    GivenUp -> []

instance ToJSON Goal where
  toJSON (Goal {workspaceId, goalId ,description,status,actionStats } ) = object [
            "workspaceId" .= workspaceId,
            "goalId" .= goalId,
            "description" .= description,
            "status" .= status,
            "actionStats" .= actionStats]

instance FromJSON Goal  where

    parseJSON (Object jsonObject) =
      Goal
        <$> jsonObject .: "workspaceId"
        <*>  jsonObject .: "goalId"
        <*>  jsonObject .: "description"
        <*>  jsonObject .: "status"
        <*>  jsonObject .: "actionStats"
    parseJSON _ =  error $ "Json format not expected"


instance ToJSON GoalStatus where
    toJSON (Created) = String "created"
    toJSON (InProgress) = String "inProgress"
    toJSON (Paused) = String "paused"
    toJSON (Accomplished) = String "accomplished"
    toJSON (GivenUp) = String "givenUp"


instance FromJSON GoalStatus  where

  parseJSON (String s) = case unpack s of
          "created" -> return Created
          "inProgress" -> return InProgress
          "paused" -> return Paused
          "accomplished" -> return Accomplished
          "givenUp" -> return GivenUp
          _ -> error $ "FromJSON GoalStatus : Json format not expected"
  parseJSON _ =  error $ "FromJSON GoalStatus : Json format not expected"