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
module Gsd.Write.API.Definition where

import Servant
import Cqrs.Write.PersistCommandResult
import Gsd.Write.Commands.Command
import Cqrs.Write.Aggregate.Ids.AggregateId
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import PersistedStreamEngine.Interface.PersistedItem
import System.SafeResponse

type GsdWriteApi =   SendGsdCommand :<|>  WaitTillCommandResponseProduced

type SendGsdCommand = "gsd" :> "write" :> "sendCommand"
                                       :> ReqBody '[JSON] GsdCommand
                                       :> PostAccepted '[JSON] PersistCommandResult


type WaitTillCommandResponseProduced = "gsd" :> "write" :> "waitTillCommandResponseProduced"
                                       :> Capture "agreggateId" AggregateId
                                       :> Capture "offset" Offset
                                       :> Capture "commandId" CommandId
                                       :> Get '[JSON] (SafeResponse (Persisted CommandResponse))