{-# LANGUAGE DeriveFunctor #-}
module Cqrs.EDsl (
  CommandDirective (..),
  RejectionReason,
  CommandTransaction,
  Action (..),
  persistEvent,updateValidationState,getNewEventID,getCurrentTime
 ) where

import Cqrs.Aggregate.Events.Event
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import qualified Data.Time as Time
import Control.Monad.Free
import Cqrs.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Aggregate.Events.EventId

data CommandDirective = Reject RejectionReason | SkipBecauseAlreadyProcessed | Transact (CommandTransaction ())



data Action a = PersistEvent Event a
              | UpdateValidationState ValidationState a
              | GetCurrentTime (Time.UTCTime -> a )
              | GetNewEventId (EventId -> a) deriving (Functor)

type CommandTransaction a = Free Action a

persistEvent :: Event -> CommandTransaction ()
persistEvent event = Free (PersistEvent event (Pure ()))

updateValidationState :: ValidationState -> CommandTransaction ()
updateValidationState validationState = Free (UpdateValidationState validationState (Pure ()))

getNewEventID :: CommandTransaction EventId
getNewEventID = Free (GetNewEventId Pure)

getCurrentTime :: CommandTransaction Time.UTCTime
getCurrentTime = Free (GetCurrentTime Pure)