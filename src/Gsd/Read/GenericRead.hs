{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Gsd.Read.GenericRead where

import Streamly hiding (Streaming)
import Data.Function ((&))
import qualified Streamly.Prelude as S
import Data.Maybe
import Data.Text hiding (map,length,find)
import Data.List (find)
import PersistedStreamEngine.Interface.Streamable
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.PersistedItem
import Control.Monad.IO.Class (MonadIO(liftIO))

import PersistedStreamEngine.Interface.Read.Reading
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Gsd.Read.Action
import Gsd.Write.Core
import Gsd.Write.Events.Event
import Cqrs.Write.Aggregate.Events.Event
import Cqrs.Write.Serialization.Event ()
import Gsd.Read.GoalStats
import Gsd.Read.ActionStats

import Control.Lens

data WorkspaceBuilder = WorkspaceBuilder {  workspaceIdMaybe :: Maybe WorkspaceId ,
                                            workspaceNameMaybe :: Maybe WorkspaceName ,
                                            goalStats :: GoalStats} deriving Show

streamWorkspace :: (Streamable stream monad WorkspaceId , Streamable SerialT monad Event) =>
                     AggregateIdStream persistedStream ->
                     GetEventStream persistedStream ->
                     Streaming persistedStream ->
                     stream monad (SafeResponse (Persisted Workspace))
streamWorkspace aggregateIdStream getEventStream streaming @ Streaming {streamAll} =
    (streamAll $ aggregateIdStream)
      & mapMSafe (\PersistedItem { offset = offset, item = workspaceId} ->
          (fmap . fmap ) (\workspace -> PersistedItem {offset, item = workspace })
            $ fetchWorkspace
                getEventStream
                streaming
                workspaceId)
      & S.filter (\maybePersistedWorkspace -> case maybePersistedWorkspace of
        Just persistedWorkspace -> True
        Nothing -> False )
      & S.map (\maybePersistedWorkspace -> fromJust $ maybePersistedWorkspace)

mapMSafe :: (IsStream t, MonadAsync m) => (a -> m (SafeResponse b)) -> t m (SafeResponse a) -> t m (SafeResponse b)
mapMSafe transformation stream = stream & S.mapM (\safeResponse -> case safeResponse of
    Right a -> transformation a
    Left error -> return $ Left error )

filter

fetchWorkspace :: Streamable SerialT monad Event =>
                     GetEventStream persistedStream ->
                     Streaming persistedStream ->
                     WorkspaceId ->
                     monad (Maybe Workspace)
fetchWorkspace getEventStream streaming @ Streaming {streamAll} workspaceId =
    S.foldx
     foldEvent
     WorkspaceBuilder { workspaceIdMaybe = Nothing, workspaceNameMaybe = Nothing,
                        goalStats = GoalStats {total = 0, accomplished = 0, toBeAccomplished = 0 }}
     (\WorkspaceBuilder{workspaceIdMaybe,workspaceNameMaybe, goalStats} ->
        case (workspaceIdMaybe, workspaceNameMaybe) of
          (Just workspaceId, Just workspaceName) ->
             Just Workspace {
                     workspaceId = fromJust $ workspaceIdMaybe,
                     workspaceName = fromJust $ workspaceNameMaybe,
                     goalStats}
          otherwise -> Nothing)
     (streamingEvent getEventStream streaming workspaceId)
  where

    streamingEvent :: (Streamable stream monad Event) =>
                        GetEventStream persistedStream ->
                        Streaming persistedStream ->
                        WorkspaceId ->
                        stream monad GsdEvent
    streamingEvent getEventStream Streaming {streamAll} workspaceId =
        (streamAll $ getEventStream workspaceId) & S.map (\PersistedItem{item = event} -> fromEvent event)

    foldEvent :: WorkspaceBuilder -> GsdEvent -> WorkspaceBuilder
    foldEvent workspaceBuilder gsdEvent =
      case (workspaceBuilder,gsdEvent) of
         (WorkspaceBuilder {..} , WorkspaceCreated {workspaceId}) ->
            WorkspaceBuilder { workspaceIdMaybe = Just workspaceId, ..}
         (WorkspaceBuilder {..} , WorkspaceNamed {workspaceName}) ->
            WorkspaceBuilder { workspaceNameMaybe = Just workspaceName, ..}
         (WorkspaceBuilder {..} , WorkspaceRenamed {workspaceNewName}) ->
            WorkspaceBuilder { workspaceNameMaybe = Just workspaceNewName, ..}
         (WorkspaceBuilder {goalStats = GoalStats {total,toBeAccomplished, ..}, .. }, GoalSet {}) ->
            WorkspaceBuilder { goalStats =  GoalStats {
                                                total = total + 1,
                                                toBeAccomplished = toBeAccomplished +1 , ..} , ..}
         (WorkspaceBuilder {goalStats = GoalStats {total,toBeAccomplished, ..}, .. }, GoalGivenUp {}) ->
            WorkspaceBuilder { goalStats =  GoalStats {
                                                toBeAccomplished = toBeAccomplished -1 , ..} , ..}
         (WorkspaceBuilder {goalStats = GoalStats {total,toBeAccomplished, accomplished}, ..} , GoalAccomplished {}) ->
            WorkspaceBuilder { goalStats =  GoalStats {
                                                toBeAccomplished = toBeAccomplished -1,
                                                accomplished = accomplished +1 , .. } , ..}
         (workspaceBuilder , _ ) -> workspaceBuilder

streamGoal :: Streamable stream monad Event =>
                GetEventStream persistedStream ->
                Streaming persistedStream ->
                WorkspaceId ->
                stream monad Goal
streamGoal getEventStream streaming workspaceId = do
    goals <- liftIO $ fetchGoals getEventStream streaming workspaceId
    S.fromList goals

fetchGoal :: Streamable SerialT monad Event =>
                     GetEventStream persistedStream ->
                     Streaming persistedStream ->
                     WorkspaceId ->
                     GoalId ->
                     monad (Maybe Goal)
fetchGoal getEventStream streaming @ Streaming {streamAll} workspaceId goalIdGiven =
  (find (\goal@Goal{goalId} -> goalId == goalIdGiven)) <$> (fetchGoals getEventStream streaming workspaceId)

fetchGoals :: Streamable SerialT monad Event =>
                     GetEventStream persistedStream ->
                     Streaming persistedStream ->
                     WorkspaceId ->
                     monad [Goal]
fetchGoals getEventStream streaming @ Streaming {streamAll} workspaceId =
  S.foldx
    folding
    []
    id
    (streamingEvent getEventStream streaming workspaceId)
  where
    streamingEvent :: (Streamable stream monad Event) =>
                        GetEventStream persistedStream ->
                        Streaming persistedStream ->
                        WorkspaceId ->
                        stream monad GsdEvent
    streamingEvent getEventStream Streaming {streamAll} workspaceId =
      (streamAll $ getEventStream workspaceId) & S.map (\PersistedItem{item = event} -> fromEvent event)

    folding :: [Goal] -> GsdEvent -> [Goal]
    folding goals GoalSet {workspaceId, goalId, goalDescription} =
      goals ++ [Goal { workspaceId,
                       goalId,
                       description = goalDescription,
                       status = Created,
                       actionStats = ActionStats {total = 0, completed = 0,opened = 0}}]
    folding goals GoalDescriptionRefined {goalId, refinedGoalDescription} =
      updateGoalDescription goalId refinedGoalDescription goals
      where
        updateGoalDescription :: GoalId -> Text -> [Goal] -> [Goal]
        updateGoalDescription goalIdToUpdate refinedGoalDescription goals =
          map (\goal@Goal{..} -> case (goalIdToUpdate == goalId) of
            True -> Goal{description = refinedGoalDescription, ..}
            False -> goal
          ) $ goals
    folding goals GoalStarted {goalId} = updateGoalStatus goalId InProgress goals
    folding goals GoalPaused {goalId} = updateGoalStatus goalId Paused goals
    folding goals GoalAccomplished {goalId = goalIdToUpdate} =
      map (\goal@Goal{actionStats = ActionStats {total,completed,opened, ..} , ..} -> case (goalIdToUpdate == goalId) of
                           True -> Goal{actionStats = ActionStats {opened = 0 , completed = total, ..}, ..}
                           False -> goal) $ updateGoalStatus goalIdToUpdate Accomplished goals
    folding goals GoalGivenUp {goalId = goalIdToUpdate} =
      map (\goal@Goal{actionStats = ActionStats {total,completed,opened, ..} , ..} -> case (goalIdToUpdate == goalId) of
                     True -> Goal{actionStats = ActionStats {opened = 0 , completed = total, ..}, ..}
                     False -> goal) $ updateGoalStatus goalIdToUpdate GivenUp goals
    folding goals ActionRevealed {goalId = goalIdToUpdate} =
      map (\goal@Goal{actionStats = ActionStats {total,opened, ..} , ..} -> case (goalIdToUpdate == goalId) of
               True -> Goal{actionStats = ActionStats {total = total +1,opened = opened + 1 , ..}, ..}
               False -> goal
             ) $ goals
    folding goals ActionCompleted {goalId = goalIdToUpdate} =
      map (\goal@Goal{actionStats = ActionStats {opened,completed, ..} , ..} -> case (goalIdToUpdate == goalId) of
                     True -> Goal{actionStats = ActionStats {completed = completed +1,opened = opened - 1 , .. }, ..}
                     False -> goal
                   ) $ goals
    folding goals gsdEvent = goals

    updateGoalStatus :: GoalId -> GoalStatus -> [Goal] -> [Goal]
    updateGoalStatus goalIdToUpdate newGoalStatus goals =
      map (\goal@Goal{..} -> case (goalIdToUpdate == goalId) of
        True -> Goal{status = newGoalStatus, ..}
        False -> goal
      ) $ goals


streamAction :: Streamable stream monad Event =>
                  GetEventStream persistedStream ->
                  Streaming persistedStream ->
                  WorkspaceId ->
                  GoalId ->
                  stream monad Action
streamAction getEventStream streaming workspaceId goalIdGiven = do
    actions <- liftIO $ S.foldx
      folding
      []
      id
      (streamingEvent getEventStream streaming workspaceId goalIdGiven)

    S.fromList actions
  where
    streamingEvent :: (Streamable stream monad Event) =>
                        GetEventStream persistedStream ->
                        Streaming persistedStream ->
                        WorkspaceId -> GoalId ->
                        stream monad GsdEvent
    streamingEvent getEventStream Streaming {streamAll} workspaceId goalIdToStream =
      (streamAll $ getEventStream workspaceId) & S.map (\PersistedItem{item = event} -> fromEvent event)
    folding :: [Action] -> GsdEvent -> [Action]
    folding actions ActionRevealed {actionDetails, ..} | goalIdGiven == goalId =
      actions ++ [Action { indexation = (length actions), details = actionDetails, status = Initiated, ..}]
    folding actions ActionCompleted {workspaceId,goalId,actionId} | goalIdGiven == goalId =
      updateActions actionId Completed actions
      where
        updateActions :: ActionId -> ActionStatus -> [Action] -> [Action]
        updateActions actionIdToUpdate actionStatus actions =
          map (\action@Action{..} -> case (actionIdToUpdate == actionId) of
            True -> Action{status = actionStatus, ..}
            False -> action
          ) $ actions
    folding actions GoalAccomplished {goalId} | goalIdGiven == goalId =
      map (\action@Action{..} -> Action{status = Completed, ..}) $ actions
    folding actions GoalGivenUp {goalId} | goalIdGiven == goalId =
          map (\action@Action{..} -> Action{status = Completed, ..}) $ actions
    folding actions gsdEvent = actions






