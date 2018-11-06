module Cqrs.PersistedCommand where

import Cqrs.Command
import Cqrs.Core
import Cqrs.Streams

data PersistedCommand = PersistedCommand {
                                offset :: Offset ,
                                command :: Command}


instance Show PersistedCommand where
  show PersistedCommand { offset = offset , command = Command { commandHeader = commandHeader }} =
    "PersistedCommand { offset = " ++ ( show $ offset) ++ " , aggregateId = " ++ (show $ aggregateId $ commandHeader) ++ " , command = " ++ (show $ commandName commandHeader) ++ ":"
    ++ (show $ aggregateId commandHeader) ++ " }"

getAggregateId :: PersistedCommand -> AggregateId
getAggregateId persistedCommand = aggregateId $ commandHeader $ command persistedCommand