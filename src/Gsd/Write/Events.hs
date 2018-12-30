{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Write.Events where


import Gsd.Write.Core
import Cqrs.Write.Aggregate.Events.Event
import Cqrs.Write.Aggregate.Events.EventId
import qualified Cqrs.Write.Aggregate.Events.Event as CoreEvent
import Data.Time
import Data.Text
import qualified Data.Map as Map
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.Maybe

data GsdEvent = WorkspaceCreated { workspaceId ::WorkspaceId , eventId :: EventId , createdOn :: UTCTime  }
              | WorkspaceNamed { workspaceId ::WorkspaceId , workspaceName :: Text , eventId :: EventId , createdOn :: UTCTime }
    deriving (Eq,Show)

eventNameForWorkspaceCreated :: String
eventNameForWorkspaceCreated = "workspaceCreated"

eventNameForWorkspaceNamed :: String
eventNameForWorkspaceNamed = "workspaceNamed"

isCreateWorkspaceEvent :: Event -> Bool
isCreateWorkspaceEvent event = (eventName $ eventHeader event) == eventNameForWorkspaceCreated

toEvent :: GsdEvent -> Event
toEvent  WorkspaceCreated {eventId, workspaceId, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = eventNameForWorkspaceCreated } ,
            payload = Map.empty}
toEvent  WorkspaceNamed {eventId, workspaceId, workspaceName, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = eventNameForWorkspaceNamed } ,
            payload = Map.fromList [("workspaceName",  String workspaceName ) ]}


fromEvent :: Event -> GsdEvent
fromEvent event =
  case (eventName $ eventHeader event) of
    "workspaceCreated" -> WorkspaceCreated {eventId = CoreEvent.eventId $ eventHeader event,
                                                 workspaceId = aggregateId $ eventHeader event,
                                                 createdOn = CoreEvent.createdOn $ eventHeader event }
    "workspaceNamed" -> WorkspaceNamed {eventId = CoreEvent.eventId $ eventHeader event,
                                             workspaceId = aggregateId $ eventHeader event,
                                             createdOn = CoreEvent.createdOn $ eventHeader event,
                                             workspaceName =  fromJust $ (fromJust $ (Map.lookup "workspaceName" (payload event))) ^? _String  }
    _ -> error "error from event"
