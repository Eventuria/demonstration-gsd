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

module Gsd.Monitoring.WebServer  where


import Servant


import Streamly.Adapters
import Servant.Pipes ()
import qualified Pipes as P
import qualified Pipes.Prelude as P.Prelude

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
import Control.Exception.Safe hiding (Handler)
import Data.String.Conversions (cs)

webServer :: EventStoreSettings  -> Server GSDMonitoringStreamingApi
webServer eventStoreSettings @ EventStoreSettings {logger} =
    healthCheck
     :<|> streamWorkspaceId
     :<|> streamCommand
     :<|> streamInfinitelyCommand
     :<|> streamCommandResponse
     :<|> streamEvent
     :<|> streamInfinitelyEvent
     :<|> streamGsdValidationStateByWorkspaceId
  where

    healthCheck :: Handler HealthCheckResult
    healthCheck = return Healthy

    streamWorkspaceId :: Handler (P.Producer (Persisted WorkspaceId) IO ())
    streamWorkspaceId = return $ toPipes $ GsdMonitoring.streamWorkspaceId eventStoreSettings

    streamCommandResponse :: WorkspaceId -> Handler (P.Producer (Persisted CommandResponse) IO ())
    streamCommandResponse workspaceId = return $ toPipes $ GsdMonitoring.streamCommandResponse
                                                              eventStoreSettings
                                                              workspaceId

    streamInfinitelyCommand :: WorkspaceId -> Handler (P.Producer (Persisted GsdCommand) IO ())
    streamInfinitelyCommand workspaceId = return $ toPipes $ GsdMonitoring.streamInfinitelyCommand
                                                                eventStoreSettings
                                                                workspaceId

    streamCommand :: WorkspaceId -> Handler (P.Producer (Persisted GsdCommand) IO ())
    streamCommand workspaceId =
        return $ redirectGsdErrorsToServantErrors $ toPipes $ GsdMonitoring.streamCommand eventStoreSettings workspaceId


    streamEvent :: WorkspaceId -> Handler (P.Producer (Persisted GsdEvent) IO ())
    streamEvent workspaceId = return $ toPipes $ GsdMonitoring.streamEvent eventStoreSettings workspaceId

    streamInfinitelyEvent :: WorkspaceId -> Handler (P.Producer (Persisted GsdEvent) IO ())
    streamInfinitelyEvent workspaceId = return $ toPipes $ GsdMonitoring.streamInfinitelyEvent
                                                              eventStoreSettings
                                                              workspaceId

    streamGsdValidationStateByWorkspaceId :: WorkspaceId ->
                                             Handler (P.Producer (Persisted (ValidationState GsdState)) IO ())
    streamGsdValidationStateByWorkspaceId workspaceId = return $ toPipes $ GsdMonitoring.streamValidationState
                                                                              eventStoreSettings
                                                                              workspaceId


    redirectGsdErrorsToServantErrors:: P.Producer (Either SomeException (Persisted GsdCommand)) IO () -> P.Producer (Persisted GsdCommand) IO ()
    redirectGsdErrorsToServantErrors producer = producer P.>-> (P.Prelude.mapM (\item -> case item of
                    Right persistedItem -> return persistedItem
                    Left (SomeException e) -> throwError $ userError $ show e))

    handleInternalError :: Handler a -> Handler a
    handleInternalError = handleAny throwEx
        where
          throwEx e = throwError $ err500 {errBody = "The following server exception occured: " <> (cs $ show e)}