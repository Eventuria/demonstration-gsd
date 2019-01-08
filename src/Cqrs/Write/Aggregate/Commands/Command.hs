{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Cqrs.Write.Aggregate.Commands.Command where

import Data.Aeson

import Data.Map
import Cqrs.Write.Aggregate.Core
import Cqrs.Write.Aggregate.Commands.CommandId
import GHC.Generics
import Cqrs.Write.Aggregate.Commands.CommandHeader


data Command = Command { commandHeader :: CommandHeader,
                         payload :: CommandPayload} deriving (Eq,Show,Generic)

type CommandPayload = Map String Value

class CommandJoinable a where
 getCommandId :: a -> CommandId

instance AggregateJoinable Command where
  getAggregateId Command { commandHeader = CommandHeader {aggregateId = aggregateId} } = aggregateId




