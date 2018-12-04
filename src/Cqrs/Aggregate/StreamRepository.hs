module Cqrs.Aggregate.StreamRepository where

import Cqrs.Aggregate.Events.Event
import Plugins.GregYoungEventStore.Stream
import Cqrs.Aggregate.Commands.Command
import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState

type EventStream = EventStoreStream Event
type CommandStream = EventStoreStream Command
type AggregateIdStream = EventStoreStream AggregateId
type CommandResponseStream = EventStoreStream CommandResponse
type ValidateStateStream = EventStoreStream ValidationState

type GetCommandStream = (AggregateId -> CommandStream)
type GetCommandResponseStream = (AggregateId -> CommandResponseStream)
type GetValidateStateStream = (AggregateId -> ValidateStateStream)
type GetEventStream = (AggregateId -> EventStream)

data EventStoreStreamRepository = StreamRepository {
                                      aggregateIdStream :: AggregateIdStream,
                                      getCommandStream :: GetCommandStream,
                                      getCommandResponseStream :: GetCommandResponseStream,
                                      getValidationStateStream :: GetValidateStateStream,
                                      getEventStream :: GetEventStream
                                    }