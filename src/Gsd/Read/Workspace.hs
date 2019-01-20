{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.Read.Workspace where

import Gsd.Write.Core
import Data.Aeson
import Data.Text


data GoalStats = GoalStats {total :: Integer, accomplished :: Integer,  toBeAccomplished :: Integer } deriving Show
data ActionStats = ActionStats {total :: Integer, completed :: Integer,  opened :: Integer } deriving Show

type WorkspaceName = Text
data Workspace = Workspace { workspaceId :: WorkspaceId , workspaceName :: WorkspaceName , goalStats :: GoalStats,  actionStats :: ActionStats  } deriving Show

instance ToJSON Workspace where
  toJSON (Workspace {workspaceId, workspaceName ,goalStats,actionStats } ) = object [
            "workspaceId" .= workspaceId,
            "workspaceName" .= workspaceName,
            "goalStats" .= goalStats,
            "actionStats" .= actionStats]

instance FromJSON Workspace  where

    parseJSON (Object jsonObject) = Workspace <$> jsonObject .: "workspaceId" <*>  jsonObject .: "workspaceName" <*>  jsonObject .: "goalStats" <*>  jsonObject .: "actionStats"
    parseJSON _ =  error $ "Json format not expected"


instance ToJSON GoalStats where
  toJSON (GoalStats {total, accomplished ,toBeAccomplished } ) = object [
            "total" .= total,
            "accomplished" .= accomplished,
            "toBeAccomplished" .= toBeAccomplished]

instance FromJSON GoalStats  where

    parseJSON (Object jsonObject) = GoalStats <$> jsonObject .: "total" <*>  jsonObject .: "accomplished" <*>  jsonObject .: "toBeAccomplished"
    parseJSON _ =  error $ "Json format not expected"

instance ToJSON ActionStats where
  toJSON (ActionStats {total, completed ,opened } ) = object [
            "total" .= total,
            "completed" .= completed,
            "opened" .= opened]

instance FromJSON ActionStats  where

    parseJSON (Object jsonObject) = ActionStats <$> jsonObject .: "total" <*>  jsonObject .: "completed" <*>  jsonObject .: "opened"
    parseJSON _ =  error $ "Json format not expected"