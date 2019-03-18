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

module Eventuria.GSD.Read.API.Definition where

import Servant
import qualified Pipes as P
import Eventuria.GSD.Read.Model.Workspace
import Eventuria.GSD.Read.Model.Goal
import Eventuria.GSD.Read.Model.Action
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.GSD.Write.Model.Core
import Eventuria.Commons.Dependencies.Core

type GSDReadApi =   HealthCheckRequest
                      :<|> StreamWorkspace
                      :<|>   StreamGoal
                      :<|>   StreamAction
                      :<|>   FetchWorkspace
                      :<|>   FetchGoal

type HealthCheckRequest =      "health" :> Get '[JSON]  Healthy

type StreamWorkspace =   "gsd" :> "read" :> "streamWorkspace"
                                         :> StreamGet NewlineFraming JSON (P.Producer (Persisted Workspace) IO () )

type FetchWorkspace =   "gsd" :> "read" :> "fetchWorkspace"
                                        :> Capture "workspaceId" WorkspaceId
                                        :> Get '[JSON] (Maybe Workspace)

type StreamGoal =        "gsd" :> "read" :> "streamGoal"
                                         :> Capture "workspaceId" WorkspaceId
                                         :> "goals" :> StreamGet NewlineFraming JSON (P.Producer Goal IO () )

type FetchGoal =   "gsd" :> "read" :> "fetchGoal"
                                        :> Capture "workspaceId" WorkspaceId
                                        :> Capture "goalId" GoalId
                                        :> Get '[JSON] (Maybe Goal)

type StreamAction =      "gsd" :> "read" :> "streamAction"
                                         :> Capture "workspaceId" WorkspaceId
                                         :> "goals"
                                         :> Capture "goalId" GoalId
                                         :> StreamGet NewlineFraming JSON (P.Producer Action IO () )

