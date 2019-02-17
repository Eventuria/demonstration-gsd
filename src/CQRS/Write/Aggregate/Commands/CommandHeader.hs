{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module CQRS.Write.Aggregate.Commands.CommandHeader where

import CQRS.Write.Aggregate.Ids.AggregateId
import CQRS.Write.Aggregate.Commands.CommandId
import GHC.Generics

type CommandName = String

data CommandHeader =  CommandHeader { aggregateId :: AggregateId,
                               commandId :: CommandId ,
                               commandName :: CommandName} deriving (Eq,Show,Generic)




