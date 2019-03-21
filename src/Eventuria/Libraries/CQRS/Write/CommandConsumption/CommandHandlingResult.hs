{-# LANGUAGE DeriveGeneric #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult where

import Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
import GHC.Generics

type RejectionReason = String

data CommandHandlingResult =   CommandRejected   { reason ::  RejectionReason}
                             | CommandValidated  { events ::  [Event]} deriving (Eq,Generic,Show)


