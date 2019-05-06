{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.HandleGSDCommand (handleGSDCommand) where

import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Definitions
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult

import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Write.Model.Commands.Mapper
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.CreateWorkspace          as CreateWorkspace
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.RenameWorkspace          as RenameWorkspace
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.SetGoal                  as SetGoal
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.RefineGoalDescription    as RefineGoalDescription
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.StartWorkingOnGoal       as StartWorkingOnGoal
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.PauseWorkingOnGoal       as PauseWorkingOnGoal
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.NotifyGoalAccomplishment as NotifyGoalAccomplishment
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.GiveUpOnGoal             as GiveUpOnGoal
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.ActionizeOnGoal          as ActionizeOnGoal
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.NotifyActionCompleted    as NotifyActionCompleted



type GSDCommandHandler  = Maybe GsdWriteModel -> (Persisted GSDCommand) -> IO (CommandHandlingResult)


handleGSDCommand :: HandleCommand GsdWriteModel
handleGSDCommand writeModelMaybe PersistedItem {offset , item = command } = gsdCommandHandler writeModelMaybe PersistedItem {offset , item = (fromCommand command) }



gsdCommandHandler :: GSDCommandHandler
gsdCommandHandler Nothing           PersistedItem {offset , item = (GSDCommand CreateWorkspaceRep          (createWorkspace         ))} = CreateWorkspace.handle                     createWorkspace
gsdCommandHandler (Just writeModel) PersistedItem {offset , item = (GSDCommand RenameWorkspaceRep          (renameWorkspace         ))} = RenameWorkspace.handle          writeModel renameWorkspace
gsdCommandHandler (Just writeModel) PersistedItem {offset , item = (GSDCommand SetGoalRep                  (setGoal                 ))} = SetGoal.handle                  writeModel setGoal
gsdCommandHandler (Just writeModel) PersistedItem {offset , item = (GSDCommand RefineGoalDescriptionRep    (refineGoalDescription   ))} = RefineGoalDescription.handle    writeModel refineGoalDescription
gsdCommandHandler (Just writeModel) PersistedItem {offset , item = (GSDCommand StartWorkingOnGoalRep       (startWorkingOnGoal      ))} = StartWorkingOnGoal.handle       writeModel startWorkingOnGoal
gsdCommandHandler (Just writeModel) PersistedItem {offset , item = (GSDCommand PauseWorkingOnGoalRep       (pauseWorkingOnGoal      ))} = PauseWorkingOnGoal.handle       writeModel pauseWorkingOnGoal
gsdCommandHandler (Just writeModel) PersistedItem {offset , item = (GSDCommand NotifyGoalAccomplishmentRep (notifyGoalAccomplishment))} = NotifyGoalAccomplishment.handle writeModel notifyGoalAccomplishment
gsdCommandHandler (Just writeModel) PersistedItem {offset , item = (GSDCommand GiveUpOnGoalRep             (giveUpOnGoal            ))} = GiveUpOnGoal.handle             writeModel giveUpOnGoal
gsdCommandHandler (Just writeModel) PersistedItem {offset , item = (GSDCommand ActionizeOnGoalRep          (actionizeOnGoal         ))} = ActionizeOnGoal.handle          writeModel actionizeOnGoal
gsdCommandHandler (Just writeModel) PersistedItem {offset , item = (GSDCommand NotifyActionCompletedRep    (notifyActionCompleted   ))} = NotifyActionCompleted.handle    writeModel notifyActionCompleted
gsdCommandHandler _ _ = return $ CommandRejected "Scenario not handle"





