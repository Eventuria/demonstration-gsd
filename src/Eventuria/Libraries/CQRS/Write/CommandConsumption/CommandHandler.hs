{-# LANGUAGE DeriveFunctor #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandler  where

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.EDsl
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState



type CommandHandler applicationState = (Persisted Command) -> Maybe (ValidationState applicationState) -> CommandDirective applicationState




