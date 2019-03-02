module Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler  where

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.ResponseDSL
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState

type CommandHandler applicationState = (Persisted Command) ->
                                       Maybe (ValidationState applicationState) ->
                                       CommandHandlingResponse applicationState




