{-# LANGUAGE DeriveFunctor #-}
module CQRS.Write.CommandConsumption.CommandHandler  where

import PersistedStreamEngine.Interface.PersistedItem
import CQRS.Write.Aggregate.Commands.Command
import CQRS.EDsl
import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState



type CommandHandler applicationState = (Persisted Command) -> Maybe (ValidationState applicationState) -> CommandDirective applicationState




