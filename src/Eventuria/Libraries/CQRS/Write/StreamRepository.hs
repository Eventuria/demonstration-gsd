module Eventuria.Libraries.CQRS.Write.StreamRepository where

import Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState

type EventStream persistedStream = persistedStream Event
type CommandStream persistedStream = persistedStream Command
type AggregateIdStream persistedStream = persistedStream AggregateId
type CommandResponseStream persistedStream = persistedStream CommandResponse
type ValidateStateStream persistedStream applicationState = persistedStream (ValidationState applicationState)

type GetCommandStream persistedStream = (AggregateId -> CommandStream persistedStream)
type GetCommandResponseStream persistedStream = (AggregateId -> CommandResponseStream persistedStream)
type GetValidationStateStream persistedStream applicationState = (AggregateId -> ValidateStateStream persistedStream applicationState)
type GetEventStream persistedStream = (AggregateId -> EventStream persistedStream)

data CQRSStreamRepository persistedStream applicationState = CQRSStreamRepository {
                                      aggregateIdStream :: AggregateIdStream persistedStream,
                                      getCommandStream :: GetCommandStream persistedStream,
                                      getCommandResponseStream :: GetCommandResponseStream persistedStream,
                                      getValidationStateStream :: GetValidationStateStream persistedStream applicationState,
                                      getEventStream :: GetEventStream persistedStream
                                    }