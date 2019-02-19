{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}


module Gsd.CLI.Workflow.Steps where

import System.Console.Byline
import Data.Text
import System.Exit (exitSuccess)
import Gsd.CLI.Dependencies
import Gsd.Read.Model.Workspace
import Gsd.Read.Model.Goal
import Control.Monad.IO.Class (MonadIO(liftIO))


type ErrorDescription = String

type WorkOnWorkspacesStepHandle = Dependencies ->
                                  Byline IO ()

type WorkOnAWorkspaceStepHandle = Dependencies ->
                                  Workspace ->
                                  WorkOnWorkspacesStepHandle ->
                                  Byline IO ()
type WorkOnAGoalStepHandle      = Dependencies ->
                                  Workspace ->
                                  Goal ->
                                  WorkOnAWorkspaceStepHandle ->
                                  WorkOnWorkspacesStepHandle ->
                                  Byline IO ()


data StepType = WorkOnWorkspaces
              | WorkOnAWorkspace
              | WorkOnAGoal
              | Quit

data Step stepType where
  WorkOnWorkspacesStep :: WorkOnWorkspacesStepHandle ->
                          Dependencies ->
                          Step WorkOnWorkspaces
  WorkOnAWorkspaceStep :: WorkOnAWorkspaceStepHandle ->
                          Dependencies ->
                          Workspace ->
                          WorkOnWorkspacesStepHandle ->
                          Step WorkOnAWorkspace
  WorkOnAGoalStep      :: WorkOnAGoalStepHandle ->
                          Dependencies ->
                          Workspace ->
                          Goal ->
                          WorkOnAWorkspaceStepHandle ->
                          WorkOnWorkspacesStepHandle ->
                          Step WorkOnAGoal
  QuitStep :: Step Quit

data StepError = forall stepType. StepError { currentStep :: Step stepType , errorDescription :: ErrorDescription}

runNextStep :: forall stepType. Either StepError (Step stepType) -> Byline IO ()
runNextStep nextStepEither = case nextStepEither of
  Right (WorkOnWorkspacesStep workOnWorkspaces
                              cliDependencies ) -> workOnWorkspaces
                                            cliDependencies
  Right (WorkOnAWorkspaceStep workOnWorkspace
                              cliDependencies
                              workspace
                              workOnWorkspaces) -> workOnWorkspace
                                                    cliDependencies
                                                    workspace
                                                    workOnWorkspaces
  Right (WorkOnAGoalStep     workOnAGoal
                             cliDependencies
                             workspace
                             goal
                             workOnWorkspace
                             workOnWorkspaces) -> workOnAGoal
                                                    cliDependencies
                                                    workspace
                                                    goal
                                                    workOnWorkspace
                                                    workOnWorkspaces
  Right (QuitStep ) -> liftIO $ exitSuccess
  Left  StepError {currentStep,errorDescription} -> do
      sayLn $ fg red <> "Error: " <>  (text . pack ) errorDescription
      sayLn $ ""
      runNextStep $ Right currentStep

