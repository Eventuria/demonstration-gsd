{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command where

import Data.Aeson
import Data.Map

import GHC.Generics

import Eventuria.Libraries.CQRS.Write.Aggregate.Core
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader


data Command = Command { commandHeader :: CommandHeader,
                         payload :: CommandPayload} deriving (Eq,Show,Generic)

type CommandPayload = Map String Value

class CommandJoinable a where
 getCommandId :: a -> CommandId

instance AggregateJoinable Command where
  getAggregateId Command { commandHeader = CommandHeader {aggregateId = aggregateId} } = aggregateId




