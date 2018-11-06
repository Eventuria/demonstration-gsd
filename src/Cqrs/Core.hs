
module Cqrs.Core where
import Data.UUID
import Data.Time


type CommandId = UUID
type AggregateId = UUID
type CommandName = String
type EventId = UUID
type EventName = String