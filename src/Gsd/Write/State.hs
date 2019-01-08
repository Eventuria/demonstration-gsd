{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Gsd.Write.State where


import Data.Aeson
import GHC.Generics
import Gsd.Write.Core
import Data.Text

type GoalDescription = Text
data Goal = Goal {workspaceId ::WorkspaceId , goalId :: GoalId , description :: GoalDescription} deriving (Show , Eq , Generic )

data GsdState = GsdState { goals :: [Goal]} deriving (Show,Eq,Generic)

instance ToJSON GsdState where
  toJSON (GsdState {goals } ) = object [
            "goals" .= goals]

instance FromJSON GsdState  where

    parseJSON (Object jsonObject) = GsdState <$> jsonObject .: "goals"
    parseJSON _ =  error $ "Json format not expected"


instance ToJSON Goal where
  toJSON (Goal {workspaceId, goalId ,description } ) = object [
            "workspaceId" .= workspaceId,
            "goalId" .= goalId,
            "description" .= description]

instance FromJSON Goal  where

    parseJSON (Object jsonObject) = Goal <$> jsonObject .: "workspaceId" <*>  jsonObject .: "goalId" <*>  jsonObject .: "description"
    parseJSON _ =  error $ "Json format not expected"