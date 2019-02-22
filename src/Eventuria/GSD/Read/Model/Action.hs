{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Eventuria.GSD.Read.Model.Action where

import Data.Aeson
import Eventuria.GSD.Write.Model.Core
import Data.Text
import GHC.Generics

type ActionDetails = Text

data ActionStatus = Initiated
                  | Completed deriving (Show , Eq , Generic )

data Action = Action {workspaceId :: WorkspaceId ,
                      goalId :: GoalId ,
                      actionId :: ActionId ,
                      indexation :: Int,
                      details :: ActionDetails ,
                      status :: ActionStatus} deriving (Show , Eq , Generic )


instance ToJSON Action where
  toJSON (Action {workspaceId, goalId ,actionId,indexation , details,status } ) = object [
            "workspaceId" .= workspaceId,
            "goalId" .= goalId,
            "actionId" .= actionId,
            "indexation" .= indexation,
            "details" .= details,
            "status" .= status]

instance FromJSON Action  where

    parseJSON (Object jsonObject) = Action
      <$> jsonObject .: "workspaceId"
      <*>  jsonObject .: "goalId"
      <*>  jsonObject .: "actionId"
      <*>  jsonObject .: "indexation"
      <*>  jsonObject .: "details"
      <*>  jsonObject .: "status"
    parseJSON _ =  error $ "FromJSON Action : Json format not expected"


instance ToJSON ActionStatus where
    toJSON (Initiated) = String "initiated"
    toJSON (Completed) = String "completed"


instance FromJSON ActionStatus  where
  parseJSON (String s) = case unpack s of
    "initiated" -> return Initiated
    "completed" -> return Completed
    _ -> error $ "Json format not expected"
  parseJSON _ =  error $ "FromJSON ActionStatus : Json format not expected"