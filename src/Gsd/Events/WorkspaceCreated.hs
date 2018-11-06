{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.Events.WorkspaceCreated where

import Gsd.CreateWorkspace.Command
import qualified Gsd.CreateWorkspace.Command as CreateWorkspaceModule
import Gsd.Core
import Cqrs.Core
import Cqrs.Events
import qualified Cqrs.Events as CoreEvent
import Data.Time
import Data.Aeson
import qualified Data.Text as Text

data WorkspaceCreated = WorkspaceCreated { workspaceId ::WorkspaceId , eventId :: EventId , createdOn :: UTCTime  } deriving (Eq,Show)


workspaceCreated :: EventId -> UTCTime -> CreateWorkspace -> WorkspaceCreated
workspaceCreated eventId createdOn createWorkspace =
  WorkspaceCreated {  eventId = eventId ,
                      createdOn = createdOn,
                      workspaceId = CreateWorkspaceModule.workspaceId createWorkspace}


eventNameForWorkspaceCreated = "workspaceCreated" :: String

isCreateWorkspaceEvent :: Event -> Bool
isCreateWorkspaceEvent event = (eventName $ eventHeader event) == eventNameForWorkspaceCreated

toEvent :: WorkspaceCreated -> Event
toEvent  WorkspaceCreated {eventId = eventId, workspaceId = workspaceId, createdOn = createdOn} =
  Event { eventHeader = EventHeader { eventId = eventId,
                            aggregateId = workspaceId ,
                            createdOn = createdOn,
                            eventName = eventNameForWorkspaceCreated } ,
            payload = []}

fromEvent :: Event -> Maybe WorkspaceCreated
fromEvent event =
  case (eventName $ eventHeader event) of
    "workspaceCreated" -> Just WorkspaceCreated {eventId = CoreEvent.eventId $ eventHeader event,
                                                 workspaceId = aggregateId $ eventHeader event,
                                                 createdOn = CoreEvent.createdOn $ eventHeader event }
    _ -> Nothing
