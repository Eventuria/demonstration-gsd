module Eventuria.Libraries.CQRS.Write.StreamRepository where


import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandTransaction


type CommandStream persistedStream = persistedStream Command
type AggregateIdStream persistedStream = persistedStream AggregateId
type CommandTransactionStream persistedStream = persistedStream CommandTransaction

type GetCommandStream persistedStream = (AggregateId -> CommandStream persistedStream)
type GetCommandTransactionStream persistedStream = (AggregateId -> CommandTransactionStream persistedStream)


data CQRSWriteStreamRepository persistedStream writeModel = CQRSWriteStreamRepository {
                                      aggregateIdStream           :: AggregateIdStream           persistedStream,
                                      getCommandStream            :: GetCommandStream            persistedStream,
                                      getCommandTransactionStream :: GetCommandTransactionStream persistedStream
                                    }