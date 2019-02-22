{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState where

import Data.Set (Set)
import GHC.Generics
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.Aggregate.Core

data ValidationState state = ValidationState { lastOffsetConsumed :: Offset , commandsProcessed :: Set CommandId, aggregateId :: AggregateId , state :: Maybe state } deriving (Eq,Generic, Show)

instance AggregateJoinable (ValidationState state) where
  getAggregateId ValidationState { aggregateId } = aggregateId







