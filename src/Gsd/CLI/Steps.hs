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
import Gsd.Clients
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Control.Monad.IO.Class (MonadIO(liftIO))


type ErrorDescription = String

type WorkOnWorkspacesStepHandle = ClientsSetting ->
                                  Byline IO ()

type WorkOnAWorkspaceStepHandle = ClientsSetting ->
                                  Workspace ->
                                  WorkOnWorkspacesStepHandle ->
                                  Byline IO ()
type WorkOnAGoalStepHandle      = ClientsSetting ->
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
                          ClientsSetting ->
                          Step WorkOnWorkspaces
  WorkOnAWorkspaceStep :: WorkOnAWorkspaceStepHandle ->
                          ClientsSetting ->
                          Workspace ->
                          WorkOnWorkspacesStepHandle ->
                          Step WorkOnAWorkspace
  WorkOnAGoalStep      :: WorkOnAGoalStepHandle ->
                          ClientsSetting ->
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
                              clients ) -> workOnWorkspaces
                                            clients
  Right (WorkOnAWorkspaceStep workOnWorkspace
                              clients
                              workspace
                              workOnWorkspaces) -> workOnWorkspace
                                                    clients
                                                    workspace
                                                    workOnWorkspaces
  Right (WorkOnAGoalStep     workOnAGoal
                             clients
                             workspace
                             goal
                             workOnWorkspace
                             workOnWorkspaces) -> workOnAGoal
                                                    clients
                                                    workspace
                                                    goal
                                                    workOnWorkspace
                                                    workOnWorkspaces
  Right (QuitStep ) -> liftIO $ exitSuccess
  Left  StepError {currentStep,errorDescription} -> do
      sayLn $ fg red <> "Error: " <>  (text . pack ) errorDescription
      sayLn $ ""
      runNextStep $ Right currentStep

