{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Eventuria.GSD.Write.Model.Events.Serialization where

import Data.Aeson
import Eventuria.GSD.Write.Model.Events.Event
import qualified Data.Text as Text


instance ToJSON GsdEvent where
  toJSON (WorkspaceCreated {eventId, createdOn, workspaceId  } ) = object [
            "eventId" .= eventId,
            "createdOn" .= createdOn,
            "workspaceId" .= workspaceId,
            "eventName" .= workspaceCreatedEventName]
  toJSON (WorkspaceNamed {eventId, createdOn, workspaceId,workspaceName  } ) = object [
              "eventId" .= eventId,
              "createdOn" .= createdOn,
              "workspaceId" .= workspaceId,
              "workspaceName" .= workspaceName,
              "eventName" .= workspaceNamedEventName]
  toJSON (WorkspaceRenamed {eventId, createdOn, workspaceId,workspaceNewName  } ) = object [
              "eventId" .= eventId,
              "createdOn" .= createdOn,
              "workspaceId" .= workspaceId,
              "workspaceNewName" .= workspaceNewName,
              "eventName" .= workspaceRenamedEventName]
  toJSON (GoalSet {eventId, createdOn, workspaceId,goalId,goalDescription  } ) = object [
                "eventId" .= eventId,
                "createdOn" .= createdOn,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "goalDescription" .= goalDescription,
                "eventName" .= goalSetEventName]
  toJSON (GoalDescriptionRefined {eventId, createdOn, workspaceId,goalId,refinedGoalDescription  } ) = object [
                "eventId" .= eventId,
                "createdOn" .= createdOn,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "refinedGoalDescription" .= refinedGoalDescription,
                "eventName" .= goalDescriptionRefinedEventName]
  toJSON (GoalStarted {eventId, createdOn, workspaceId,goalId  } ) = object [
                "eventId" .= eventId,
                "createdOn" .= createdOn,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "eventName" .= goalStartedEventName]
  toJSON (GoalPaused {eventId, createdOn, workspaceId,goalId  } ) = object [
                "eventId" .= eventId,
                "createdOn" .= createdOn,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "eventName" .= goalPausedEventName]
  toJSON (GoalAccomplished {eventId, createdOn, workspaceId,goalId  } ) = object [
                "eventId" .= eventId,
                "createdOn" .= createdOn,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "eventName" .= goalAccomplishedEventName]
  toJSON (GoalGivenUp {eventId, createdOn, workspaceId,goalId,reason  } ) = object [
                "eventId" .= eventId,
                "createdOn" .= createdOn,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "reason" .= reason,
                "eventName" .= goalGivenUpEventName]
  toJSON (ActionRevealed {eventId, createdOn, workspaceId,goalId,actionId,actionDetails  } ) = object [
                "eventId" .= eventId,
                "createdOn" .= createdOn,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "actionId" .= actionId,
                "actionDetails" .= actionDetails,
                "eventName" .= actionRevealedEventName]
  toJSON (ActionCompleted {eventId, createdOn, workspaceId,goalId,actionId  } ) = object [
                "eventId" .= eventId,
                "createdOn" .= createdOn,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "actionId" .= actionId,
                "eventName" .= actionCompletedEventName]




instance FromJSON GsdEvent where

  parseJSON (Object jsonObject) = do
               commandNameMaybe <- jsonObject .: "eventName"
               case commandNameMaybe of
                    Just (String commandName) | (Text.unpack commandName) == workspaceCreatedEventName ->
                      WorkspaceCreated
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                    Just (String commandName) | (Text.unpack commandName) == workspaceNamedEventName ->
                      WorkspaceNamed
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "workspaceName"
                    Just (String commandName) | (Text.unpack commandName) == workspaceRenamedEventName ->
                      WorkspaceRenamed
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "workspaceNewName"
                    Just (String commandName) | (Text.unpack commandName) == goalSetEventName ->
                      GoalSet
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                          <*> jsonObject .: "goalDescription"
                    Just (String commandName) | (Text.unpack commandName) == goalDescriptionRefinedEventName ->
                      GoalDescriptionRefined
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                          <*> jsonObject .: "refinedGoalDescription"
                    Just (String commandName) | (Text.unpack commandName) == goalStartedEventName ->
                      GoalStarted
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                    Just (String commandName) | (Text.unpack commandName) == goalPausedEventName ->
                      GoalPaused
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                    Just (String commandName) | (Text.unpack commandName) == goalAccomplishedEventName ->
                      GoalAccomplished
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                    Just (String commandName) | (Text.unpack commandName) == goalGivenUpEventName ->
                      GoalGivenUp
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                          <*> jsonObject .: "reason"
                    Just (String commandName) | (Text.unpack commandName) == actionRevealedEventName ->
                      ActionRevealed
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                          <*> jsonObject .: "actionId"
                          <*> jsonObject .: "actionDetails"
                    Just (String commandName) | (Text.unpack commandName) == actionCompletedEventName ->
                      ActionCompleted
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                          <*> jsonObject .: "actionId"
                    Just (String unknownCommandName) -> error $ "Command unknown : " ++ Text.unpack unknownCommandName
                    _ -> error $ "Command name not provided"
  parseJSON _ =  error $ "Json format not expected"
