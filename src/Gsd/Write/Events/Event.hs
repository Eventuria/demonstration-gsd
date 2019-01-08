{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Gsd.Write.Events.Event where


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
import Data.UUID
import Control.Monad (join)
import GHC.Generics

data GsdEvent = WorkspaceCreated { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId    }
              | WorkspaceNamed   { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId , workspaceName :: Text }
              | WorkspaceRenamed { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId , workspaceNewName :: Text }
              | GoalSet { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId , goalId :: GoalId , goalDescription :: Text}

    deriving (Eq,Show,Generic)

eventNameForWorkspaceCreated :: String
eventNameForWorkspaceCreated = "workspaceCreated"

eventNameForWorkspaceNamed :: String
eventNameForWorkspaceNamed = "workspaceNamed"

eventNameForWorkspaceRenamed :: String
eventNameForWorkspaceRenamed = "workspaceRenamed"

eventNameForGoalSet :: String
eventNameForGoalSet = "goalSet"



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
toEvent  WorkspaceRenamed {eventId, workspaceId, workspaceNewName, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = eventNameForWorkspaceRenamed } ,
            payload = Map.fromList [("workspaceNewName",  String workspaceNewName ) ]}
toEvent  GoalSet {eventId, workspaceId, goalId, goalDescription, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = eventNameForGoalSet } ,
            payload = Map.fromList [("goalId",  String $ toText goalId ),("goalDescription",  String goalDescription ) ]}



fromEvent :: Event -> GsdEvent
fromEvent event =
  case (eventName $ eventHeader event) of
    "workspaceCreated" -> WorkspaceCreated {eventId = CoreEvent.eventId $ eventHeader event,
                                                 workspaceId = aggregateId $ eventHeader event,
                                                 createdOn = CoreEvent.createdOn $ eventHeader event }
    "workspaceNamed" -> WorkspaceNamed {eventId = CoreEvent.eventId $ eventHeader event,
                                             workspaceId = aggregateId $ eventHeader event,
                                             createdOn = CoreEvent.createdOn $ eventHeader event,
                                             workspaceName =  extractPayloadTextValue event "workspaceName"  }
    "workspaceRenamed" -> WorkspaceRenamed {eventId = CoreEvent.eventId $ eventHeader event,
                                           workspaceId = aggregateId $ eventHeader event,
                                           createdOn = CoreEvent.createdOn $ eventHeader event,
                                           workspaceNewName =  extractPayloadTextValue event "workspaceNewName"  }
    "goalSet" -> GoalSet {eventId = CoreEvent.eventId $ eventHeader event,
                                               workspaceId = aggregateId $ eventHeader event,
                                               createdOn = CoreEvent.createdOn $ eventHeader event,
                                               goalId =  extractPayloadUUIDValue event "goalId",
                                               goalDescription = extractPayloadTextValue event "goalDescription" }
    _ -> error "error from event"


extractPayloadTextValue :: Event -> String -> Text
extractPayloadTextValue event key = fromJust $ (fromJust $ (Map.lookup key (payload event))) ^? _String

extractPayloadUUIDValue :: Event -> String -> UUID
extractPayloadUUIDValue event key = fromJust $ join $ (fromString . unpack) <$> (fromJust $ (Map.lookup key (payload event))) ^? _String