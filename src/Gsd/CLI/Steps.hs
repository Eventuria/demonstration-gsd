{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}


module Gsd.CLI.Steps where

import System.Console.Byline
import Data.Text
import System.Exit (exitSuccess)
import Gsd.CLI.State
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Control.Monad.IO.Class (MonadIO(liftIO))


type ErrorDescription = String

type WorkOnWorkspacesStepHandle = State ->
                                  Byline IO ()

type WorkOnAWorkspaceStepHandle = State ->
                                  Workspace ->
                                  WorkOnWorkspacesStepHandle ->
                                  Byline IO ()
type WorkOnAGoalStepHandle      = State ->
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
                          State ->
                          Step WorkOnWorkspaces
  WorkOnAWorkspaceStep :: WorkOnAWorkspaceStepHandle ->
                          State ->
                          Workspace ->
                          WorkOnWorkspacesStepHandle ->
                          Step WorkOnAWorkspace
  WorkOnAGoalStep      :: WorkOnAGoalStepHandle ->
                          State ->
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
                              state ) -> workOnWorkspaces
                                            state
  Right (WorkOnAWorkspaceStep workOnWorkspace
                              state
                              workspace
                              workOnWorkspaces) -> workOnWorkspace
                                                    state
                                                    workspace
                                                    workOnWorkspaces
  Right (WorkOnAGoalStep     workOnAGoal
                             state
                             workspace
                             goal
                             workOnWorkspace
                             workOnWorkspaces) -> workOnAGoal
                                                    state
                                                    workspace
                                                    goal
                                                    workOnWorkspace
                                                    workOnWorkspaces
  Right (QuitStep ) -> liftIO $ exitSuccess
  Left  StepError {currentStep,errorDescription} -> do
      sayLn $ fg red <> "Error: " <>  (text . pack ) errorDescription
      sayLn $ ""
      runNextStep $ Right currentStep

