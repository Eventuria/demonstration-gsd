{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState where

import Data.Set (Set)
import GHC.Generics
import PersistedStreamEngine.Interface.Offset
import Cqrs.Write.Aggregate.Commands.CommandId
import Cqrs.Write.Aggregate.Ids.AggregateId
import Cqrs.Write.Aggregate.Core

data ValidationState state = ValidationState { lastOffsetConsumed :: Offset , commandsProcessed :: Set CommandId, aggregateId :: AggregateId , state :: Maybe state } deriving (Eq,Generic, Show)

instance AggregateJoinable (ValidationState state) where
  getAggregateId ValidationState { aggregateId } = aggregateId







