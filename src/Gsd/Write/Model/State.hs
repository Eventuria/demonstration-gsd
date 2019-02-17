{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Gsd.Write.Model.State where


import Data.Aeson
import GHC.Generics
import Gsd.Write.Model.Core
import Data.Text hiding (index)
import Data.Set

type GoalDescription = Text

data GoalStatus = Created | InProgress | Paused | Accomplished | GivenUp deriving (Show , Eq , Generic )


data ActionStatus = Initiated | Completed deriving (Show , Eq , Generic )

data Action = Action {actionId :: ActionId, index :: Int , details :: Text ,status :: ActionStatus } deriving (Show , Eq , Generic )

instance Ord Action where
  (<=) a b =  (index a) <= (index b)

data Goal = Goal {workspaceId ::WorkspaceId ,
                  goalId :: GoalId ,
                  description :: GoalDescription ,
                  actions :: Set Action,
                  status :: GoalStatus} deriving (Show , Eq , Generic )


data GsdState = GsdState { goals :: [Goal]} deriving (Show,Eq,Generic)


instance ToJSON GsdState where
  toJSON (GsdState {goals } ) = object [
            "goals" .= goals]

instance FromJSON GsdState  where

    parseJSON (Object jsonObject) = GsdState <$> jsonObject .: "goals"
    parseJSON _ =  error $ "Json format not expected"


instance ToJSON Goal where
  toJSON (Goal {workspaceId, goalId ,description,actions, status } ) = object [
            "workspaceId" .= workspaceId,
            "goalId" .= goalId,
            "description" .= description,
            "actions" .= actions,
            "status" .= status]

instance FromJSON Action  where

    parseJSON (Object jsonObject) = Action <$> jsonObject .: "actionId" <*>  jsonObject .: "index" <*>  jsonObject .: "details" <*>  jsonObject .: "status"
    parseJSON _ =  error $ "Json format not expected"

instance ToJSON Action where
  toJSON (Action {actionId, index, details, status } ) = object [
            "actionId" .= actionId,
            "index" .= index,
            "details" .= details,
            "status" .= status]

instance ToJSON ActionStatus where
    toJSON (Initiated) = String "initiated"
    toJSON (Completed) = String "completed"

instance FromJSON ActionStatus  where

  parseJSON (String s) = case unpack s of
          "initiated" -> return Initiated
          "completed" -> return Completed
          _ -> error $ "FromJSON ActionStatus : Json format not expected"
  parseJSON _ =  error $ "FromJSON ActionStatus : Json format not expected"

instance FromJSON Goal  where

    parseJSON (Object jsonObject) = Goal <$> jsonObject .: "workspaceId" <*>  jsonObject .: "goalId" <*>  jsonObject .: "description" <*>  jsonObject .: "actions" <*>  jsonObject .: "status"
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