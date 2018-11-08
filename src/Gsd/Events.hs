{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.Events where

import Gsd.Commands
import Gsd.Core
import Cqrs.Core
import Cqrs.Events
import qualified Cqrs.Events as CoreEvent
import Data.Time
import Data.Aeson
import qualified Data.Text as Text

data GsdEvent = WorkspaceCreated { workspaceId ::WorkspaceId , eventId :: EventId , createdOn :: UTCTime  } deriving (Eq,Show)


eventNameForWorkspaceCreated = "workspaceCreated" :: String

isCreateWorkspaceEvent :: Event -> Bool
isCreateWorkspaceEvent event = (eventName $ eventHeader event) == eventNameForWorkspaceCreated

toEvent :: GsdEvent -> Event
toEvent  WorkspaceCreated {eventId = eventId, workspaceId = workspaceId, createdOn = createdOn} =
  Event { eventHeader = EventHeader { eventId = eventId,
                            aggregateId = workspaceId ,
                            createdOn = createdOn,
                            eventName = eventNameForWorkspaceCreated } ,
            payload = []}

fromEvent :: Event -> Maybe GsdEvent
fromEvent event =
  case (eventName $ eventHeader event) of
    "workspaceCreated" -> Just WorkspaceCreated {eventId = CoreEvent.eventId $ eventHeader event,
                                                 workspaceId = aggregateId $ eventHeader event,
                                                 createdOn = CoreEvent.createdOn $ eventHeader event }
    _ -> Nothing
