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
module Eventuria.GSD.Write.CommandSourcer.Definition where

import Servant
import Eventuria.Libraries.CQRS.Write.PersistCommandResult
import Eventuria.GSD.Write.Model.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Commons.Dependencies.Core
import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandTransaction


type GsdWriteApi =  HealthCheckRequest :<|>  SendGsdCommand :<|>  WaitTillCommandResponseProduced

type HealthCheckRequest =      "health" :> Get '[JSON]  Healthy

type SendGsdCommand = "gsd" :> "write" :> "sendCommand"
                                       :> ReqBody '[JSON] GSDCommand
                                       :> PostAccepted '[JSON] PersistCommandResult


type WaitTillCommandResponseProduced = "gsd" :> "write" :> "waitTillCommandResponseProduced"
                                       :> Capture "agreggateId" AggregateId
                                       :> Capture "offset" Offset
                                       :> Capture "commandId" CommandId
                                       :> Get '[JSON] (Persisted CommandTransaction)