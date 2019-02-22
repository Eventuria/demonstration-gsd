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

module Gsd.Monitoring.API.Definition where

import Servant
import Gsd.Write.Model.Core
import Gsd.Write.Model.Events.Event
import Gsd.Write.Model.Commands.Command
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Write.Model.State
import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import Eventuria.Commons.DevOps.Core
import qualified Pipes as P
import Eventuria.Commons.System.SafeResponse


type GSDMonitoringStreamingApi =   HealthCheck
                             :<|>  StreamGsdCommandsByWorkspaceId
                             :<|>  StreamInfinitelyGsdCommandsByWorkspaceId
                             :<|>  StreamGsdCommandResponseByWorkspaceId
                             :<|>  StreamGsdEventsByWorkspaceId
                             :<|>  StreamInfinitelyGsdEventsByWorkspaceId
                             :<|>  StreamGsdValidationStateByWorkspaceId


type HealthCheck =      "health" :> Get '[JSON]  HealthCheckResult

type StreamGsdCommandsByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "command" :>
                                      Capture "workspaceId" WorkspaceId :>
                                      StreamGet
                                        NewlineFraming
                                        JSON
                                        (P.Producer (SafeResponse (Persisted GsdCommand)) IO () )

type StreamInfinitelyGsdCommandsByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "infinitely" :> "commands" :>
                                                Capture "workspaceId" WorkspaceId :>
                                                StreamGet
                                                  NewlineFraming
                                                  JSON
                                                  (P.Producer (SafeResponse (Persisted GsdCommand)) IO () )

type StreamGsdCommandResponseByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "commandResponses" :>
                                             Capture "workspaceId" WorkspaceId :>
                                             StreamGet
                                              NewlineFraming
                                              JSON
                                              (P.Producer (SafeResponse (Persisted CommandResponse)) IO () )

type StreamGsdEventsByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "events" :>
                                    Capture "workspaceId" WorkspaceId :>
                                    StreamGet
                                      NewlineFraming
                                      JSON
                                      (P.Producer (SafeResponse (Persisted GsdEvent)) IO () )

type StreamInfinitelyGsdEventsByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "infinitely" :> "events" :>
                                              Capture "workspaceId" WorkspaceId :>
                                              StreamGet
                                                NewlineFraming
                                                JSON
                                                (P.Producer (SafeResponse (Persisted GsdEvent)) IO () )

type StreamGsdValidationStateByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "validationState" :>
                                             Capture "workspaceId" WorkspaceId :>
                                             StreamGet
                                              NewlineFraming
                                              JSON
                                              (P.Producer (SafeResponse (Persisted (ValidationState GsdState))) IO () )