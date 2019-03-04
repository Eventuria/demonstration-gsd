{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler  where

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event

type CommandHandler writeModel = Maybe writeModel ->
                                 (Persisted Command) ->
                                 IO (CommandHandlerResult writeModel)

type RejectionReason = String

data CommandHandlingResponse = RejectCommand   { rejectionReason ::  RejectionReason}
                             | ValidateCommand { events          ::  [Event]} deriving Show

data CommandHandlerResult writeModel = CommandHandlerResult {
                                            writeModelMaybe :: Maybe writeModel,
                                            result :: CommandHandlingResponse } deriving Show


rejectCommand :: Maybe writeModel -> RejectionReason  -> CommandHandlerResult writeModel
rejectCommand writeModelMaybe rejectionReason = CommandHandlerResult {
                    writeModelMaybe,
                    result = RejectCommand rejectionReason}

validateCommand :: writeModel -> [Event]  -> CommandHandlerResult writeModel
validateCommand writeModel events = CommandHandlerResult {
                    writeModelMaybe = Just writeModel,
                    result = ValidateCommand {events}}

