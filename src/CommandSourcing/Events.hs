{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CommandSourcing.Events where

import CommandSourcing.Core
import Data.UUID
import Data.Aeson
import Data.Attoparsec.Internal.Types
import Data.Time
import qualified Data.Text as Text

data WorkspaceEvent = WorkspaceCreated { createdOn :: UTCTime , eventId :: UUID , commandId :: UUID , workspaceId ::WorkspaceId }
           | IdeaIntroduced   { createdOn :: UTCTime , eventId :: UUID , commandId :: UUID , workspaceId ::WorkspaceId, ideaContent :: String } deriving (Eq,Show)


serializedEventNameForWorkspaceCreated :: String
serializedEventNameForWorkspaceCreated = "workspaceCreated"

serializedEventNameForIdeaIntroduced :: String
serializedEventNameForIdeaIntroduced = "ideaIntroduced"

class EventSerializable event where
  serializedEventName :: event -> String


instance EventSerializable WorkspaceEvent where
  serializedEventName WorkspaceCreated {} = serializedEventNameForWorkspaceCreated
  serializedEventName IdeaIntroduced {} = serializedEventNameForIdeaIntroduced


instance ToJSON WorkspaceEvent where
  toJSON (event @ (WorkspaceCreated createdOn eventId commandId workspaceId )) = object [
            "createdOn" .= createdOn,
            "eventId" .= eventId,
            "commandId" .= commandId,
            "workspaceId" .= workspaceId,
            "eventName" .= serializedEventName event]

  toJSON (event @ (IdeaIntroduced createdOn eventId commandId workspaceId ideaContent)) = object [
          "createdOn" .= createdOn,
          "eventId" .= eventId,
          "commandId" .= commandId,
          "workspaceId" .= workspaceId,
          "ideaContent" .= ideaContent,
          "eventName" .= serializedEventName event]

instance FromJSON WorkspaceEvent where

    parseJSON (Object jsonObject) = do
                 eventNameMaybe <- jsonObject .: "eventName"
                 case eventNameMaybe of
                      Just (String eventName) | (Text.unpack eventName) == serializedEventNameForWorkspaceCreated -> WorkspaceCreated
                          <$> jsonObject .: "createdOn"
                          <*> jsonObject .: "eventId"
                          <*> jsonObject .: "commandId"
                          <*> jsonObject .: "workspaceId"
                      Just (String eventName) | (Text.unpack eventName) == serializedEventNameForIdeaIntroduced -> IdeaIntroduced
                          <$> jsonObject .: "createdOn"
                          <*> jsonObject .: "eventId"
                          <*> jsonObject .: "commandId"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "ideaContent"
                      Just (String unknownEventName) -> error $ "Event unknown : " ++ Text.unpack unknownEventName
                      Nothing -> error $ "Event name not provided"