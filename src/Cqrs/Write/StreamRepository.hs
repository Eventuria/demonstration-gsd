module Cqrs.Write.StreamRepository where

import Cqrs.Write.Aggregate.Events.Event
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.Aggregate.Ids.AggregateId
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState

type EventStream persistedStream = persistedStream Event
type CommandStream persistedStream = persistedStream Command
type AggregateIdStream persistedStream = persistedStream AggregateId
type CommandResponseStream persistedStream = persistedStream CommandResponse
type ValidateStateStream persistedStream = persistedStream ValidationState

type GetCommandStream persistedStream = (AggregateId -> CommandStream persistedStream)
type GetCommandResponseStream persistedStream = (AggregateId -> CommandResponseStream persistedStream)
type GetValidateStateStream persistedStream = (AggregateId -> ValidateStateStream persistedStream)
type GetEventStream persistedStream = (AggregateId -> EventStream persistedStream)

data CqrsStreamRepository persistedStream = CqrsStreamRepository {
                                      aggregateIdStream :: AggregateIdStream persistedStream,
                                      getCommandStream :: GetCommandStream persistedStream,
                                      getCommandResponseStream :: GetCommandResponseStream persistedStream,
                                      getValidationStateStream :: GetValidateStateStream persistedStream,
                                      getEventStream :: GetEventStream persistedStream
                                    }