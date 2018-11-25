{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.Events where


import Gsd.Core
import Cqrs.Aggregate.Events.Event
import Cqrs.Aggregate.Events.EventId
import qualified Cqrs.Aggregate.Events.Event as CoreEvent
import Data.Time

data GsdEvent = WorkspaceCreated { workspaceId ::WorkspaceId , eventId :: EventId , createdOn :: UTCTime  } deriving (Eq,Show)

eventNameForWorkspaceCreated :: String
eventNameForWorkspaceCreated = "workspaceCreated"

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
