{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Libraries.PersistedStreamEngine.Interface.Write.TransactionDSL where

import           Control.Monad.Free
import           Control.Exception

import           Data.Time

import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.EventId

type TransactionInterpreter applicationState a = Transaction applicationState a -> IO (Either SomeException a)

data TransactionAtom applicationState a =
                  TransactionStart (Persisted Command) a
                | PersistEvent Event a
                | PersistValidationState (ValidationState applicationState) a
                | PersistCommandResponse CommandResponse a
                | GetCurrentTime (UTCTime -> a )
                | GetNewEventId (EventId -> a)
                | TransactionEnd (Persisted Command) a deriving (Functor)

type Transaction applicationState a = Free (TransactionAtom applicationState) a

transactionStart :: (Persisted Command) -> Transaction applicationState ()
transactionStart command = Free (TransactionStart command (Pure ()))

transactionEnd :: (Persisted Command) -> Transaction applicationState ()
transactionEnd  command = Free (TransactionEnd command (Pure ()))


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

