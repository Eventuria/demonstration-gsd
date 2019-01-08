{-# LANGUAGE DeriveFunctor #-}
module Cqrs.Write.CommandConsumption.CommandHandler  where

import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.EDsl
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState



type CommandHandler applicationState = (Persisted Command) -> Maybe (ValidationState applicationState) -> CommandDirective applicationState




