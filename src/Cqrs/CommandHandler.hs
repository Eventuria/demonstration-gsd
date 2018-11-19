{-# LANGUAGE DeriveFunctor #-}
module Cqrs.CommandHandler  where

import Cqrs.PersistedCommand
import Cqrs.Snapshot
import Cqrs.Events
import Cqrs.Core
import Cqrs.EDsl


type CommandHandler = PersistedCommand -> Maybe AggregateSnapshot -> CommandDirective




