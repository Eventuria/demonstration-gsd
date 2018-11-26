{-# LANGUAGE DeriveFunctor #-}
module Cqrs.CommandHandler  where

import Cqrs.EventStore.PersistedItem
import Cqrs.Aggregate.Commands.Command
import Cqrs.EDsl
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState



type CommandHandler = (Persisted Command) -> Maybe ValidationState -> CommandDirective




