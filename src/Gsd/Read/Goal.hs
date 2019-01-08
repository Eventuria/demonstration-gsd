{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Gsd.Read.Goal where

import Data.Aeson
import Gsd.Write.Core
import Data.Text
import GHC.Generics

type GoalDescription = Text

data Goal = Goal {workspaceId ::WorkspaceId , goalId :: GoalId , description :: GoalDescription} deriving (Show , Eq , Generic )

instance ToJSON Goal where
  toJSON (Goal {workspaceId, goalId ,description } ) = object [
            "workspaceId" .= workspaceId,
            "goalId" .= goalId,
            "description" .= description]

instance FromJSON Goal  where

    parseJSON (Object jsonObject) = Goal <$> jsonObject .: "workspaceId" <*>  jsonObject .: "goalId" <*>  jsonObject .: "description"
    parseJSON _ =  error $ "Json format not expected"