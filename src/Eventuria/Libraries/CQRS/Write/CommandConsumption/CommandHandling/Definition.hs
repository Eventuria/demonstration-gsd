{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandling.Definition  where

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event

type RejectionReason = String

data CommandHandlingResult =   CommandRejected   { reason ::  RejectionReason}
                             | CommandValidated  { events ::  [Event]} deriving (Eq,Show)

type ProjectWriteModel writeModel = Maybe writeModel -> CommandHandlingResult -> Maybe writeModel

type HandleCommand writeModel = Maybe writeModel -> (Persisted Command) -> IO (CommandHandlingResult)



