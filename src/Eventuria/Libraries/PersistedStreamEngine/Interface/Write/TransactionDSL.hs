{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Libraries.PersistedStreamEngine.Interface.Write.TransactionDSL where

import           Control.Monad.Free

import           Data.Time

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.EventId


data TransactionUnit applicationState a =
                  PersistEvent Event a
                | PersistValidationState (ValidationState applicationState) a
                | PersistCommandResponse CommandResponse a
                | GetCurrentTime (UTCTime -> a )
                | GetNewEventId (EventId -> a) deriving (Functor)

type Transaction applicationState a = Free (TransactionUnit applicationState) a


persistEvent :: Event ->
                Transaction applicationState ()
persistEvent event = Free (PersistEvent event (Pure ()))

persistAggregate :: ValidationState applicationState ->
                    Transaction applicationState ()
persistAggregate validationState = Free (PersistValidationState validationState (Pure ()))

persistCommandResponse :: CommandResponse ->
                          Transaction applicationState ()
persistCommandResponse commandResponse = Free (PersistCommandResponse commandResponse (Pure ()))

getNewEventID :: Transaction applicationState EventId
getNewEventID = Free (GetNewEventId Pure)

getCurrentTime :: Transaction applicationState UTCTime
getCurrentTime = Free (GetCurrentTime Pure)

