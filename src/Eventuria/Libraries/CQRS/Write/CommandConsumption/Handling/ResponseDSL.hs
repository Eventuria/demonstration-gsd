{-# LANGUAGE DeriveFunctor #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.ResponseDSL  where

import           Control.Monad.Free

import qualified Data.Time as Time

import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.EventId

data CommandHandlingResponse applicationState = RejectCommand RejectionReason
                                              | SkipCommandBecauseAlreadyProcessed
                                              | ValidateCommandWithFollowingTransactionPayload (TransactionPayload applicationState ())


data TransactionUnit applicationState a =
                PersistEvent Event a
              | UpdateValidationState (ValidationState applicationState) a
              | GetCurrentTime (Time.UTCTime -> a )
              | GetNewEventId (EventId -> a) deriving (Functor)

type TransactionPayload applicationState a = Free (TransactionUnit applicationState) a

persistEvent :: Event -> TransactionPayload applicationState ()
persistEvent event = Free (PersistEvent event (Pure ()))

updateValidationState :: (ValidationState applicationState) -> TransactionPayload applicationState ()
updateValidationState validationState = Free (UpdateValidationState validationState (Pure ()))

getNewEventID :: TransactionPayload applicationState EventId
getNewEventID = Free (GetNewEventId Pure)

getCurrentTime :: TransactionPayload applicationState Time.UTCTime
getCurrentTime = Free (GetCurrentTime Pure)