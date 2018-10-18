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


eventNameForWorkspaceCreated :: String
eventNameForWorkspaceCreated = "workspaceCreated"

eventNameForIdeaIntroduced :: String
eventNameForIdeaIntroduced = "ideaIntroduced"

class EventSerializable event where
  getEventName :: event -> String
  getEventId :: event -> UUID


instance EventSerializable WorkspaceEvent where
  getEventName WorkspaceCreated {} = eventNameForWorkspaceCreated
  getEventName IdeaIntroduced {} = eventNameForIdeaIntroduced

  getEventId WorkspaceCreated { eventId = eventId} = eventId
  getEventId IdeaIntroduced { eventId = eventId} = eventId


instance ToJSON WorkspaceEvent where
  toJSON (event @ (WorkspaceCreated createdOn eventId commandId workspaceId )) = object [
            "createdOn" .= createdOn,
            "eventId" .= eventId,
            "commandId" .= commandId,
            "workspaceId" .= workspaceId,
            "eventName" .= getEventName event]

  toJSON (event @ (IdeaIntroduced createdOn eventId commandId workspaceId ideaContent)) = object [
          "createdOn" .= createdOn,
          "eventId" .= eventId,
          "commandId" .= commandId,
          "workspaceId" .= workspaceId,
          "ideaContent" .= ideaContent,
          "eventName" .= getEventName event]

instance FromJSON WorkspaceEvent where

    parseJSON (Object jsonObject) = do
                 eventNameMaybe <- jsonObject .: "eventName"
                 case eventNameMaybe of
                      Just (String eventName) | (Text.unpack eventName) == eventNameForWorkspaceCreated -> WorkspaceCreated
                          <$> jsonObject .: "createdOn"
                          <*> jsonObject .: "eventId"
                          <*> jsonObject .: "commandId"
                          <*> jsonObject .: "workspaceId"
                      Just (String eventName) | (Text.unpack eventName) == eventNameForIdeaIntroduced -> IdeaIntroduced
                          <$> jsonObject .: "createdOn"
                          <*> jsonObject .: "eventId"
                          <*> jsonObject .: "commandId"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "ideaContent"
                      Just (String unknownEventName) -> error $ "Event unknown : " ++ Text.unpack unknownEventName
                      Nothing -> error $ "Event name not provided"