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
              | GoalDescriptionRefined { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId , goalId :: GoalId , refinedGoalDescription :: Text}
              | GoalStarted { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId , goalId :: GoalId }
              | GoalPaused { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId , goalId :: GoalId }
              | GoalAccomplished { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId , goalId :: GoalId }
              | GoalGivenUp { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId , goalId :: GoalId,reason ::Text }
              | ActionRevealed  { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId , goalId :: GoalId, actionId :: ActionId, actionDetails ::Text }
              | ActionCompleted { eventId :: EventId , createdOn :: UTCTime , workspaceId ::WorkspaceId , goalId :: GoalId, actionId :: ActionId }

    deriving (Eq,Show,Generic)

workspaceCreatedEventName :: String
workspaceNamedEventName :: String
workspaceRenamedEventName :: String
goalSetEventName :: String
goalDescriptionRefinedEventName :: String
goalStartedEventName ::String
goalPausedEventName ::String
goalAccomplishedEventName ::String
goalGivenUpEventName ::String
actionRevealedEventName ::String
actionCompletedEventName ::String

workspaceCreatedEventName = "workspaceCreated"
workspaceNamedEventName = "workspaceNamed"
workspaceRenamedEventName = "workspaceRenamed"
goalSetEventName = "goalSet"
goalDescriptionRefinedEventName = "goalDescriptionRefined"
goalStartedEventName = "goalStarted"
goalPausedEventName = "goalPaused"
goalAccomplishedEventName = "goalAccomplished"
goalGivenUpEventName = "goalGivenUp"
actionRevealedEventName = "actionRevealed"
actionCompletedEventName = "actionCompleted"

toEvent :: GsdEvent -> Event
toEvent  WorkspaceCreated {eventId, workspaceId, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = workspaceCreatedEventName } ,
            payload = Map.empty}
toEvent  WorkspaceNamed {eventId, workspaceId, workspaceName, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = workspaceNamedEventName } ,
            payload = Map.fromList [("workspaceName",  String workspaceName ) ]}
toEvent  WorkspaceRenamed {eventId, workspaceId, workspaceNewName, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = workspaceRenamedEventName } ,
            payload = Map.fromList [("workspaceNewName",  String workspaceNewName ) ]}
toEvent  GoalSet {eventId, workspaceId, goalId, goalDescription, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = goalSetEventName } ,
            payload = Map.fromList [("goalId",  String $ toText goalId ),
                                    ("goalDescription",  String goalDescription ) ]}
toEvent  GoalDescriptionRefined {eventId, workspaceId, goalId, refinedGoalDescription, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = goalDescriptionRefinedEventName } ,
            payload = Map.fromList [("goalId",  String $ toText goalId ),
                                    ("refinedGoalDescription",  String refinedGoalDescription ) ]}
toEvent  GoalStarted {eventId, workspaceId, goalId, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = goalStartedEventName } ,
            payload = Map.fromList [("goalId",  String $ toText goalId ) ]}
toEvent  GoalPaused {eventId, workspaceId, goalId, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = goalPausedEventName } ,
            payload = Map.fromList [("goalId",  String $ toText goalId ) ]}
toEvent  GoalAccomplished {eventId, workspaceId, goalId, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = goalAccomplishedEventName } ,
            payload = Map.fromList [("goalId",  String $ toText goalId ) ]}
toEvent  GoalGivenUp {eventId, workspaceId, goalId, createdOn,reason} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = goalGivenUpEventName } ,
            payload = Map.fromList [("goalId",  String $ toText goalId ),
                                    ("reason",  String reason ) ]}
toEvent  ActionRevealed {eventId, workspaceId, goalId, actionId, actionDetails,createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = actionRevealedEventName } ,
            payload = Map.fromList [("goalId",  String $ toText goalId ),
                                    ("actionId",  String $ toText actionId ),
                                    ("actionDetails",  String actionDetails ) ]}
toEvent  ActionCompleted {eventId, workspaceId, goalId, actionId, createdOn} =
  Event { eventHeader = EventHeader { eventId,
                                      aggregateId = workspaceId ,
                                      createdOn,
                                      eventName = actionCompletedEventName } ,
            payload = Map.fromList [("goalId",  String $ toText goalId ),
                                    ("actionId",  String $ toText actionId ) ]}

fromEvent :: Event -> GsdEvent
fromEvent event =
  case (eventName $ eventHeader event) of
    "workspaceCreated" -> WorkspaceCreated { eventId = CoreEvent.eventId $ eventHeader event,
                                             workspaceId = aggregateId $ eventHeader event,
                                             createdOn = CoreEvent.createdOn $ eventHeader event }
    "workspaceNamed" -> WorkspaceNamed { eventId = CoreEvent.eventId $ eventHeader event,
                                         workspaceId = aggregateId $ eventHeader event,
                                         createdOn = CoreEvent.createdOn $ eventHeader event,
                                         workspaceName =  extractPayloadTextValue event "workspaceName"  }
    "workspaceRenamed" -> WorkspaceRenamed { eventId = CoreEvent.eventId $ eventHeader event,
                                             workspaceId = aggregateId $ eventHeader event,
                                             createdOn = CoreEvent.createdOn $ eventHeader event,
                                             workspaceNewName =  extractPayloadTextValue event "workspaceNewName"  }
    "goalSet" -> GoalSet { eventId = CoreEvent.eventId $ eventHeader event,
                           workspaceId = aggregateId $ eventHeader event,
                           createdOn = CoreEvent.createdOn $ eventHeader event,
                           goalId =  extractPayloadUUIDValue event "goalId",
                           goalDescription = extractPayloadTextValue event "goalDescription" }
    "goalDescriptionRefined" -> GoalDescriptionRefined { eventId = CoreEvent.eventId $ eventHeader event,
                                                         workspaceId = aggregateId $ eventHeader event,
                                                         createdOn = CoreEvent.createdOn $ eventHeader event,
                                                         goalId =  extractPayloadUUIDValue event "goalId",
                                                         refinedGoalDescription = extractPayloadTextValue event "refinedGoalDescription" }
    "goalStarted" -> GoalStarted { eventId = CoreEvent.eventId $ eventHeader event,
                                   workspaceId = aggregateId $ eventHeader event,
                                   createdOn = CoreEvent.createdOn $ eventHeader event,
                                   goalId =  extractPayloadUUIDValue event "goalId"}
    "goalPaused" -> GoalPaused { eventId = CoreEvent.eventId $ eventHeader event,
                                 workspaceId = aggregateId $ eventHeader event,
                                 createdOn = CoreEvent.createdOn $ eventHeader event,
                                 goalId =  extractPayloadUUIDValue event "goalId"}
    "goalAccomplished" -> GoalAccomplished {eventId = CoreEvent.eventId $ eventHeader event,
                                            workspaceId = aggregateId $ eventHeader event,
                                            createdOn = CoreEvent.createdOn $ eventHeader event,
                                            goalId =  extractPayloadUUIDValue event "goalId"}
    "goalGivenUp" -> GoalGivenUp { eventId = CoreEvent.eventId $ eventHeader event,
                                   workspaceId = aggregateId $ eventHeader event,
                                   createdOn = CoreEvent.createdOn $ eventHeader event,
                                   goalId =  extractPayloadUUIDValue event "goalId",
                                   reason = extractPayloadTextValue event "reason" }
    "actionRevealed" -> ActionRevealed { eventId = CoreEvent.eventId $ eventHeader event,
                                         workspaceId = aggregateId $ eventHeader event,
                                         createdOn = CoreEvent.createdOn $ eventHeader event,
                                         goalId =  extractPayloadUUIDValue event "goalId",
                                         actionId =  extractPayloadUUIDValue event "actionId",
                                         actionDetails = extractPayloadTextValue event "actionDetails" }
    "actionCompleted" -> ActionCompleted { eventId = CoreEvent.eventId $ eventHeader event,
                                           workspaceId = aggregateId $ eventHeader event,
                                           createdOn = CoreEvent.createdOn $ eventHeader event,
                                           goalId =  extractPayloadUUIDValue event "goalId",
                                           actionId =  extractPayloadUUIDValue event "actionId" }
    _ -> error "error from event"



extractPayloadTextValue :: Event -> String -> Text
extractPayloadTextValue event key = fromJust $ (fromJust $ (Map.lookup key (payload event))) ^? _String

extractPayloadUUIDValue :: Event -> String -> UUID
extractPayloadUUIDValue event key = fromJust $ join $ (fromString . unpack) <$> (fromJust $ (Map.lookup key (payload event))) ^? _String