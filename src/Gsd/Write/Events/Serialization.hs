{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Gsd.Write.Events.Serialization where

import Data.Aeson
import Gsd.Write.Events.Event
import qualified Data.Text as Text


instance ToJSON GsdEvent where
  toJSON (WorkspaceCreated {eventId, createdOn, workspaceId  } ) = object [
            "eventId" .= eventId,
            "createdOn" .= createdOn,
            "workspaceId" .= workspaceId,
            "eventName" .= eventNameForWorkspaceCreated]
  toJSON (WorkspaceNamed {eventId, createdOn, workspaceId,workspaceName  } ) = object [
              "eventId" .= eventId,
              "createdOn" .= createdOn,
              "workspaceId" .= workspaceId,
              "workspaceName" .= workspaceName,
              "eventName" .= eventNameForWorkspaceNamed]
  toJSON (WorkspaceRenamed {eventId, createdOn, workspaceId,workspaceNewName  } ) = object [
              "eventId" .= eventId,
              "createdOn" .= createdOn,
              "workspaceId" .= workspaceId,
              "workspaceNewName" .= workspaceNewName,
              "eventName" .= eventNameForWorkspaceRenamed]
  toJSON (GoalSet {eventId, createdOn, workspaceId,goalId,goalDescription  } ) = object [
                "eventId" .= eventId,
                "createdOn" .= createdOn,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "goalDescription" .= goalDescription,
                "eventName" .= eventNameForGoalSet]
  toJSON (GoalDescriptionRefined {eventId, createdOn, workspaceId,goalId,refinedGoalDescription  } ) = object [
                "eventId" .= eventId,
                "createdOn" .= createdOn,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "refinedGoalDescription" .= refinedGoalDescription,
                "eventName" .= eventNameForGoalDescriptionRefined]



instance FromJSON GsdEvent where

  parseJSON (Object jsonObject) = do
               commandNameMaybe <- jsonObject .: "eventName"
               case commandNameMaybe of
                    Just (String commandName) | (Text.unpack commandName) == eventNameForWorkspaceCreated ->
                      WorkspaceCreated
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                    Just (String commandName) | (Text.unpack commandName) == eventNameForWorkspaceNamed ->
                      WorkspaceNamed
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "workspaceName"
                    Just (String commandName) | (Text.unpack commandName) == eventNameForWorkspaceRenamed ->
                      WorkspaceRenamed
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "workspaceNewName"
                    Just (String commandName) | (Text.unpack commandName) == eventNameForGoalSet ->
                      GoalSet
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                          <*> jsonObject .: "goalDescription"
                    Just (String commandName) | (Text.unpack commandName) == eventNameForGoalDescriptionRefined ->
                      GoalDescriptionRefined
                          <$> jsonObject .: "eventId"
                          <*> jsonObject .: "createdOn"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                          <*> jsonObject .: "refinedGoalDescription"
                    Just (String unknownCommandName) -> error $ "Command unknown : " ++ Text.unpack unknownCommandName
                    _ -> error $ "Command name not provided"
  parseJSON _ =  error $ "Json format not expected"
