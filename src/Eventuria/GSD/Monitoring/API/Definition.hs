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
import Eventuria.GSD.Write.Model.WriteModel
import Eventuria.Commons.Dependencies.Core
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse

import qualified Pipes as P


type GSDMonitoringStreamingApi =   HealthCheckRequest
                             :<|>  StreamGsdCommandsByWorkspaceId
                             :<|>  StreamGsdCommandResponseByWorkspaceId
                             :<|>  StreamGsdEventsByWorkspaceId
                             :<|>  StreamGsdWriteModelHistoryByWorkspaceId


type HealthCheckRequest = "health" :> Get '[JSON]  Healthy

type StreamGsdCommandsByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "command" :>
                                      Capture "workspaceId" WorkspaceId :>
                                      StreamGet
                                        NewlineFraming
                                        JSON
                                        (P.Producer (Persisted GSDCommand) IO () )

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


type StreamGsdWriteModelHistoryByWorkspaceId = "gsd" :> "monitoring" :> "stream" :> "writeModelHistory" :>
                                             Capture "workspaceId" WorkspaceId :>
                                             StreamGet
                                              NewlineFraming
                                              JSON
                                              (P.Producer (Persisted (Maybe GsdWriteModel)) IO () )