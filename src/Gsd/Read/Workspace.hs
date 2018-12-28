{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings     #-}
module Gsd.Read.Workspace where

import Gsd.Write.Core
import Data.Aeson

type WorkspaceName = String
data Workspace = Workspace { workspaceId :: WorkspaceId , name :: WorkspaceName } deriving Show

instance ToJSON Workspace where
  toJSON (Workspace {workspaceId, name  } ) = object [
            "workspaceId" .= workspaceId,
            "name" .= name]

instance FromJSON Workspace  where

    parseJSON (Object jsonObject) = Workspace <$> jsonObject .: "workspaceId" <*>  jsonObject .: "name"
    parseJSON _ =  error $ "Json format not expected"