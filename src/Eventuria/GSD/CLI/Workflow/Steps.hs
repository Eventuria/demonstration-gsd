{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}


module Eventuria.GSD.CLI.Workflow.Steps where

import System.Console.Byline
import System.Exit (exitSuccess)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text

import Eventuria.GSD.Read.Model.Workspace
import Eventuria.GSD.Read.Model.Goal

import Eventuria.GSD.CLI.UI.HealthChecking
import Eventuria.GSD.CLI.Dependencies

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

data StepError = forall stepType. StepError { cliDependencies :: Dependencies ,currentStep :: Step stepType , errorDescription :: ErrorDescription}

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
  Left  StepError {cliDependencies,currentStep,errorDescription} -> do
      sayLn $ fg red <> "an error occured with some dependencies : " <>  (text . pack ) errorDescription
      sayLn $ "starting health checking to diagnose...."
      liftIO $ waitTillHealthyDependencies cliDependencies
      sayLn $ "going back to the healthy flow..."
      runNextStep $ Right currentStep

