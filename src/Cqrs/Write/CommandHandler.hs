{-# LANGUAGE DeriveFunctor #-}
module Cqrs.Write.CommandHandler  where

import PersistedStreamEngine.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.EDsl
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState



type CommandHandler = (Persisted Command) -> Maybe ValidationState -> CommandDirective




