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

module Eventuria.GSD.Monitoring.API.Definition where

import Servant
import Eventuria.GSD.Write.Model.Core
import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.Commands.Command
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.GSD.Write.Model.State
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import Eventuria.Commons.DevOps.Core
import qualified Pipes as P


type GSDMonitoringStreamingApi =   HealthCheck
                             :<|>  StreamGsdCommandsByWorkspaceId
                             :<|>  StreamInfinitelyGsdCommandsByWorkspaceId
                             :<|>  StreamGsdCommandResponseByWorkspaceId
                             :<|>  StreamGsdEventsByWorkspaceId
                             :<|>  StreamInfinitelyGsdEventsByWorkspaceId
                             :<|>  StreamGsdValidationStateByWorkspaceId


type HealthCheck =      "health" :> Get '[JSON]  Healthy

type StreamGsdCommandsByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "command" :>
                                      Capture "workspaceId" WorkspaceId :>
                                      StreamGet
                                        NewlineFraming
                                        JSON
                                        (P.Producer (Persisted GsdCommand) IO () )

type StreamInfinitelyGsdCommandsByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "infinitely" :> "commands" :>
                                                Capture "workspaceId" WorkspaceId :>
                                                StreamGet
                                                  NewlineFraming
                                                  JSON
                                                  (P.Producer (Persisted GsdCommand) IO () )

type StreamGsdCommandResponseByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "commandResponses" :>
                                             Capture "workspaceId" WorkspaceId :>
                                             StreamGet
                                              NewlineFraming
                                              JSON
                                              (P.Producer (Persisted CommandResponse) IO () )

type StreamGsdEventsByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "events" :>
                                    Capture "workspaceId" WorkspaceId :>
                                    StreamGet
                                      NewlineFraming
                                      JSON
                                      (P.Producer (Persisted GsdEvent) IO () )

type StreamInfinitelyGsdEventsByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "infinitely" :> "events" :>
                                              Capture "workspaceId" WorkspaceId :>
                                              StreamGet
                                                NewlineFraming
                                                JSON
                                                (P.Producer (Persisted GsdEvent) IO () )

type StreamGsdValidationStateByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "validationState" :>
                                             Capture "workspaceId" WorkspaceId :>
                                             StreamGet
                                              NewlineFraming
                                              JSON
                                              (P.Producer (Persisted (ValidationState GsdState)) IO () )