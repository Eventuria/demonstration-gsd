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
module Gsd.Write.Flow.Sourcer.Definition where

import Servant
import CQRS.Write.PersistCommandResult
import Gsd.Write.Model.Commands.Command
import CQRS.Write.Aggregate.Ids.AggregateId
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import CQRS.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Commons.System.SafeResponse
import Eventuria.Commons.DevOps.Core

type GsdWriteApi =  HealthCheck :<|>  SendGsdCommand :<|>  WaitTillCommandResponseProduced

type HealthCheck =      "health" :> Get '[JSON]  HealthCheckResult

type SendGsdCommand = "gsd" :> "write" :> "sendCommand"
                                       :> ReqBody '[JSON] GsdCommand
                                       :> PostAccepted '[JSON] PersistCommandResult


type WaitTillCommandResponseProduced = "gsd" :> "write" :> "waitTillCommandResponseProduced"
                                       :> Capture "agreggateId" AggregateId
                                       :> Capture "offset" Offset
                                       :> Capture "commandId" CommandId
                                       :> Get '[JSON] (SafeResponse (Persisted CommandResponse))