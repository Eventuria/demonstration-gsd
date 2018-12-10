{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.Write.Aggregate.Commands.Command where

import Data.Aeson
import Data.Text

import Cqrs.Write.Aggregate.Core
import Cqrs.Write.Aggregate.Ids.AggregateId
import Cqrs.Write.Aggregate.Commands.CommandId


type Pair = (Text, Value)
type CommandName = String

data Command = Command { commandHeader :: CommandHeader,
                         payload :: CommandPayload} deriving Show

data CommandHeader =  CommandHeader { aggregateId :: AggregateId,
                               commandId :: CommandId ,
                               commandName :: CommandName} deriving Show
type CommandPayload = [Pair]

class CommandJoinable a where
 getCommandId :: a -> CommandId

instance AggregateJoinable Command where
  getAggregateId Command { commandHeader = CommandHeader {aggregateId = aggregateId} } = aggregateId




