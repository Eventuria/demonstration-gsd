{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Gsd.Monitoring.Server  where


import Servant


import Streamly.Adapters
import Servant.Pipes ()
import qualified Pipes as P
import Prelude hiding (foldr)

import Gsd.Monitoring.WebStreamingApiDefinition

import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Monitoring.MonitoringOverEventStore as GsdMonitoring

import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Write.Core

import Cqrs.Write.Serialization.PersistenceResult ()
import Cqrs.Write.Serialization.Command ()
import Cqrs.Write.Serialization.Event ()
import Gsd.Write.Commands.Command
import Gsd.Write.Commands.Serialization ()
import Gsd.Write.Events.Event
import Gsd.Write.Events.Serialization()
import Gsd.Write.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Write.Serialization.ValidationState ()
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Write.Serialization.CommandResponse ()
import DevOps.Core
import System.SafeResponse

server :: EventStoreSettings  -> Server GSDMonitoringStreamingApi
server eventStoreSettings @ EventStoreSettings {logger} =
    healthCheck
     :<|> streamCommand
     :<|> streamInfinitelyCommand
     :<|> streamCommandResponse
     :<|> streamEvent
     :<|> streamInfinitelyEvent
     :<|> streamGsdValidationStateByWorkspaceId
  where

    healthCheck :: Handler HealthCheckResult
    healthCheck = return Healthy

    streamCommandResponse :: WorkspaceId -> Handler (P.Producer (SafeResponse (Persisted CommandResponse)) IO ())
    streamCommandResponse workspaceId = return $ toPipes $ GsdMonitoring.streamCommandResponse
                                                              eventStoreSettings
                                                              workspaceId

    streamInfinitelyCommand :: WorkspaceId -> Handler (P.Producer (SafeResponse (Persisted GsdCommand)) IO ())
    streamInfinitelyCommand workspaceId = return $ toPipes $ GsdMonitoring.streamInfinitelyCommand
                                                                eventStoreSettings
                                                                workspaceId

    streamCommand :: WorkspaceId -> Handler (P.Producer (SafeResponse (Persisted GsdCommand)) IO ())
    streamCommand workspaceId =
        return $ toPipes $ GsdMonitoring.streamCommand eventStoreSettings workspaceId


    streamEvent :: WorkspaceId -> Handler (P.Producer (SafeResponse (Persisted GsdEvent)) IO ())
    streamEvent workspaceId = return $ toPipes $ GsdMonitoring.streamEvent eventStoreSettings workspaceId

    streamInfinitelyEvent :: WorkspaceId -> Handler (P.Producer (SafeResponse (Persisted GsdEvent)) IO ())
    streamInfinitelyEvent workspaceId = return $ toPipes $ GsdMonitoring.streamInfinitelyEvent
                                                              eventStoreSettings
                                                              workspaceId

    streamGsdValidationStateByWorkspaceId :: WorkspaceId ->
                                             Handler (P.Producer (SafeResponse (Persisted (ValidationState GsdState))) IO ())
    streamGsdValidationStateByWorkspaceId workspaceId = return $ toPipes $ GsdMonitoring.streamValidationState
                                                                              eventStoreSettings
                                                                              workspaceId

