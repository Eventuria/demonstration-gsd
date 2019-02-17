module CQRS.Write.StreamRepository where

import CQRS.Write.Aggregate.Events.Event
import CQRS.Write.Aggregate.Commands.Command
import CQRS.Write.Aggregate.Ids.AggregateId
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState

type EventStream persistedStream = persistedStream Event
type CommandStream persistedStream = persistedStream Command
type AggregateIdStream persistedStream = persistedStream AggregateId
type CommandResponseStream persistedStream = persistedStream CommandResponse
type ValidateStateStream persistedStream applicationState = persistedStream (ValidationState applicationState)

type GetCommandStream persistedStream = (AggregateId -> CommandStream persistedStream)
type GetCommandResponseStream persistedStream = (AggregateId -> CommandResponseStream persistedStream)
type GetValidationStateStream persistedStream applicationState = (AggregateId -> ValidateStateStream persistedStream applicationState)
type GetEventStream persistedStream = (AggregateId -> EventStream persistedStream)

data CqrsStreamRepository persistedStream applicationState = CqrsStreamRepository {
                                      aggregateIdStream :: AggregateIdStream persistedStream,
                                      getCommandStream :: GetCommandStream persistedStream,
                                      getCommandResponseStream :: GetCommandResponseStream persistedStream,
                                      getValidationStateStream :: GetValidationStateStream persistedStream applicationState,
                                      getEventStream :: GetEventStream persistedStream
                                    }