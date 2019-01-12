{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}


module Gsd.CLI.Steps where

import System.Console.Byline
import Data.Text

import Gsd.Clients
import Gsd.Read.Workspace
import Gsd.Read.Goal

type ErrorDescription = String
type WorkOnWorkspacesStepHandle = Clients -> Byline IO ()
type WorkOnAWorkspaceStepHandle = Clients -> Workspace ->  WorkOnWorkspacesStepHandle -> Byline IO ()
type WorkOnAGoalStepHandle      = Clients -> Workspace -> Goal ->  WorkOnAWorkspaceStepHandle -> WorkOnWorkspacesStepHandle -> Byline IO ()


data StepType = WorkOnWorkspaces | WorkOnAWorkspace | WorkOnAGoal

data Step stepType where
  WorkOnWorkspacesStep :: WorkOnWorkspacesStepHandle -> Clients -> Step WorkOnWorkspaces
  WorkOnAWorkspaceStep :: WorkOnAWorkspaceStepHandle -> Clients -> Workspace -> WorkOnWorkspacesStepHandle -> Step WorkOnAWorkspace
  WorkOnAGoalStep      :: WorkOnAGoalStepHandle -> Clients -> Workspace -> Goal -> WorkOnAWorkspaceStepHandle -> WorkOnWorkspacesStepHandle -> Step WorkOnAGoal


data StepError = forall stepType. StepError { currentStep :: Step stepType , errorDescription :: ErrorDescription}

runNextStep :: forall stepType. Either StepError (Step stepType) -> Byline IO ()
runNextStep nextStepEither = case nextStepEither of
  Right (WorkOnWorkspacesStep workOnWorkspaces clients ) -> workOnWorkspaces clients
  Right (WorkOnAWorkspaceStep workOnWorkspace  clients workspace workOnWorkspaces) -> workOnWorkspace clients workspace workOnWorkspaces
  Right (WorkOnAGoalStep      workOnAGoal      clients workspace goal workOnWorkspace workOnWorkspaces) -> workOnAGoal clients workspace goal workOnWorkspace workOnWorkspaces
  Left  StepError {currentStep,errorDescription} -> do
      sayLn $ fg red <> "Error: " <>  (text . pack ) errorDescription
      sayLn $ ""
      runNextStep $ Right currentStep