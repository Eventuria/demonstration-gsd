{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.Write.Client (sendCommandAndWaitResponse,SendCommandAnWaitResponse (..)) where

import Servant

import Servant.Client
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Gsd.Write.Commands.Command
import Gsd.Write.WebApi
import Cqrs.Write.Aggregate.Ids.AggregateId
import PersistedStreamEngine.Interface.Offset
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.PersistCommandResult
import System.SafeResponse


data SendCommandAnWaitResponse =  RequestFailed {reason :: String}
                        | ProcessMomentarilyPostponed {reason :: String}
                        | CommandResponseProduced CommandResponse  deriving Show

sendCommand :: GsdCommand -> ClientM PersistCommandResult
waitTillCommandResponseProduced :: AggregateId -> Offset -> CommandId -> ClientM (SafeResponse (Persisted CommandResponse))

api :: Proxy GsdWriteApi
api = Proxy

sendCommand :<|> waitTillCommandResponseProduced = client api

sendCommandAndWaitResponse :: GsdCommand -> ClientM SendCommandAnWaitResponse
sendCommandAndWaitResponse gsdCommand = do
   persistenceResult <- sendCommand gsdCommand
   case persistenceResult of
    FailedToPersist {reason } -> return $ RequestFailed {reason}
    SuccessfullyPersisted {aggregateId, commandId,lastOffsetPersisted} -> do
      safeResponse <- waitTillCommandResponseProduced aggregateId lastOffsetPersisted commandId
      case safeResponse of
        Left reason -> return $ ProcessMomentarilyPostponed {reason = show reason}
        Right PersistedItem {item = commandResponse } -> return $ CommandResponseProduced commandResponse






