{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module CQRS.Write.Aggregate.Commands.Command where

import Data.Aeson

import Data.Map
import CQRS.Write.Aggregate.Core
import CQRS.Write.Aggregate.Commands.CommandId
import GHC.Generics
import CQRS.Write.Aggregate.Commands.CommandHeader


data Command = Command { commandHeader :: CommandHeader,
                         payload :: CommandPayload} deriving (Eq,Show,Generic)

type CommandPayload = Map String Value

class CommandJoinable a where
 getCommandId :: a -> CommandId

instance AggregateJoinable Command where
  getAggregateId Command { commandHeader = CommandHeader {aggregateId = aggregateId} } = aggregateId




