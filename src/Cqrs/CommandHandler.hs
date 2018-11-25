{-# LANGUAGE DeriveFunctor #-}
module Cqrs.CommandHandler  where

import Cqrs.Aggregate.Commands.PersistedCommand
import Cqrs.EDsl
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState



type CommandHandler = PersistedCommand -> Maybe ValidationState -> CommandDirective




