{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings     #-}
module Gsd.Read.Workspace where

import Gsd.Write.Core
import Data.Aeson
import Data.Text

type WorkspaceName = Text
data Workspace = Workspace { workspaceId :: WorkspaceId , workspaceName :: WorkspaceName } deriving Show

instance ToJSON Workspace where
  toJSON (Workspace {workspaceId, workspaceName  } ) = object [
            "workspaceId" .= workspaceId,
            "workspaceName" .= workspaceName]

instance FromJSON Workspace  where

    parseJSON (Object jsonObject) = Workspace <$> jsonObject .: "workspaceId" <*>  jsonObject .: "workspaceName"
    parseJSON _ =  error $ "Json format not expected"