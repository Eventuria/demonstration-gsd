{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Gsd.Read.WebApiDefinition where

import Servant
import qualified Pipes as P
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Gsd.Read.Action
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Write.Core

type GSDReadApi =   StreamWorkspace
                      :<|>   StreamGoal
                      :<|>   StreamAction
                      :<|>   FetchWorkspace

type StreamWorkspace =   "gsd" :> "read" :> "streamWorkspace"
                                         :> StreamGet NewlineFraming JSON (P.Producer (Persisted Workspace) IO () )

type FetchWorkspace =   "gsd" :> "read" :> "fetchWorkspace"
                                        :> Capture "workspaceId" WorkspaceId
                                        :> Get '[JSON] (Maybe Workspace)

type StreamGoal =        "gsd" :> "read" :> "streamGoal"
                                         :> Capture "workspaceId" WorkspaceId
                                         :> "goals" :> StreamGet NewlineFraming JSON (P.Producer (Goal) IO () )

type StreamAction =      "gsd" :> "read" :> "streamAction"
                                         :> Capture "workspaceId" WorkspaceId
                                         :> "goals"
                                         :> Capture "goalId" GoalId
                                         :> StreamGet NewlineFraming JSON (P.Producer (Action) IO () )

