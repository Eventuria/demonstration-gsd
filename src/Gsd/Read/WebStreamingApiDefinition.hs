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

module Gsd.Read.WebStreamingApiDefinition where

import Servant
import qualified Pipes as P
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Gsd.Read.Action
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Write.Core

type GSDReadStreamingApi =   StreamWorkspace
                      :<|>   StreamGoal
                      :<|>   StreamAction

type StreamWorkspace =   "gsd" :> "read" :> "stream" :> "workspaces"
                                         :> StreamGet NewlineFraming JSON (P.Producer (Persisted Workspace) IO () )

type StreamGoal =        "gsd" :> "read" :> "stream"
                                         :> Capture "workspaceId" WorkspaceId
                                         :> "goals" :> StreamGet NewlineFraming JSON (P.Producer (Goal) IO () )

type StreamAction =      "gsd" :> "read" :> "stream"
                                         :> Capture "workspaceId" WorkspaceId
                                         :> "goals"
                                         :> Capture "goalId" GoalId
                                         :> StreamGet NewlineFraming JSON (P.Producer (Action) IO () )
