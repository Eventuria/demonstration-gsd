{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Cqrs.PersistedStream.Write.Interface where

import Data.Aeson
import Cqrs.Streams

import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.Aggregate.Events.Event
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Aggregate.Commands.Command
import Cqrs.Aggregate.Commands.Responses.CommandResponse

class ToJSON item => Writable item where
  getItemName :: item -> String


data Writing persistedStream = Writing { persist :: forall item . Writable item =>  persistedStream item -> item -> IO (Either PersistenceFailure PersistResult) }


instance Writable AggregateId where
  getItemName aggregateId  = "aggregateId"

instance Writable Event where
  getItemName Event { eventHeader = EventHeader { eventName = eventName}} = eventName

instance Writable ValidationState where
  getItemName validationState  = "validationState"

instance Writable Command where
  getItemName Command { commandHeader = CommandHeader {commandName = commandName} }  = commandName

instance Writable CommandResponse where
  getItemName CommandSuccessfullyProcessed {} = commandResponseNameForCommandSuccessfullyProcessed
  getItemName CommandSkippedBecauseAlreadyProcessed {} = commandResponseNameForCommandSkippedBecauseAlreadyProcessed
  getItemName CommandFailed {} = commandResponseNameForCommandFailed
