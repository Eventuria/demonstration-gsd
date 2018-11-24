{-# LANGUAGE DeriveFunctor #-}
module Cqrs.CommandHandler  where

import Cqrs.Commands.PersistedCommand
import Cqrs.EDsl
import Cqrs.Aggregate.Snapshots.AggregateSnapshot



type CommandHandler = PersistedCommand -> Maybe AggregateSnapshot -> CommandDirective




