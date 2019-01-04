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

module Gsd.Monitoring.WebStreamingApiDefinition where

import Servant
import Gsd.Write.Core
import Gsd.Write.Events.Event
import Gsd.Write.Commands.Command
import PersistedStreamEngine.Interface.PersistedItem

import qualified Pipes as P

type GSDMonitoringStreamingApi =   StreamWorkspaceIdsCreated
                             :<|>  StreamGsdCommandsByWorkspaceId
                             :<|>  StreamInfinitelyGsdCommandsByWorkspaceId
                             :<|>  StreamGsdEventsByWorkspaceId
                             :<|>  StreamInfinitelyGsdEventsByWorkspaceId

type StreamWorkspaceIdsCreated =      "gsd" :> "monitoring" :> "stream" :> "workspaceIds" :> StreamGet NewlineFraming JSON (P.Producer (Persisted WorkspaceId) IO () )
type StreamGsdCommandsByWorkspaceId = "gsd" :> "monitoring" :> "stream"
                                            :> "commands"
                                            :> Capture "workspaceId" WorkspaceId :> StreamGet NewlineFraming JSON (P.Producer (Persisted GsdCommand) IO () )
type StreamInfinitelyGsdCommandsByWorkspaceId = "gsd" :> "monitoring" :> "stream"
                                                      :> "infinitely"
                                                      :> "commands" :> Capture "workspaceId" WorkspaceId :> StreamGet NewlineFraming JSON (P.Producer (Persisted GsdCommand) IO () )

type StreamGsdEventsByWorkspaceId = "gsd" :> "monitoring" :> "stream"
                                          :> "events"
                                          :> Capture "workspaceId" WorkspaceId :> StreamGet NewlineFraming JSON (P.Producer (Persisted GsdEvent) IO () )
type StreamInfinitelyGsdEventsByWorkspaceId = "gsd" :> "monitoring" :> "stream"
                                                    :> "infinitely"
                                                    :> "events" :> Capture "workspaceId" WorkspaceId :> StreamGet NewlineFraming JSON (P.Producer (Persisted GsdEvent) IO () )