{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.ProjectGSDWriteModel (projectGSDWriteModel) where

import qualified Data.Set as Set

import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Events.Event

import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Definitions
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult

projectGSDWriteModel :: ProjectWriteModel GsdWriteModel
projectGSDWriteModel writeModelMaybe commandHandlingResult =
  case commandHandlingResult of
    CommandRejected {} -> writeModelMaybe
    CommandValidated events ->
      foldl
        projectWriteModelWithEvent
        writeModelMaybe
        events


projectWriteModelWithEvent ::  Maybe GsdWriteModel -> Event -> Maybe GsdWriteModel
projectWriteModelWithEvent Nothing event   = case (fromEvent event) of
  WorkspaceCreated {..} -> Just $ GsdWriteModel {goals = [] }
  gsdEvent -> error ("WorkspaceCreated should be the first event produced and not " ++ show gsdEvent )
projectWriteModelWithEvent (Just (writeModel @ GsdWriteModel {goals, ..})) event  = Just $ case (fromEvent event) of
  GoalStarted {..} -> GsdWriteModel {goals = updateGoalStatus goalId InProgress goals , ..}
  GoalSet {..}     -> GsdWriteModel {goals = (goals ++ [Goal{workspaceId,
                                                              goalId,
                                                              description = goalDescription,
                                                              actions = Set.empty ,
                                                              status = Created}]), ..}
  GoalDescriptionRefined {..} -> GsdWriteModel {goals = updateGoalWithNewRefinedGoalDescription goalId refinedGoalDescription goals, ..}
  GoalPaused {..} -> GsdWriteModel {goals = updateGoalStatus goalId Paused goals , ..}
  GoalAccomplished {..} ->  GsdWriteModel {goals = updateGoalStatus goalId Accomplished goals }
  ActionCompleted {..} ->  GsdWriteModel {goals = updateAction goals goalId actionId Completed}
  GoalGivenUp {..} -> GsdWriteModel {goals = updateGoalStatus goalId GivenUp goals }
  ActionRevealed {..} -> GsdWriteModel {goals = addAction goals goalId actionId actionDetails}
  WorkspaceNamed {..} -> writeModel
  WorkspaceRenamed {..} -> writeModel
  WorkspaceCreated {..} -> error ("WorkspaceCreated should only be the first event produced")

