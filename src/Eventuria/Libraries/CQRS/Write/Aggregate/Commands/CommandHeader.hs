{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader where

import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId

import GHC.Generics

type CommandName = String

data CommandHeader =  CommandHeader { aggregateId :: AggregateId,
                               commandId :: CommandId ,
                               commandName :: CommandName} deriving (Eq,Show,Generic)




