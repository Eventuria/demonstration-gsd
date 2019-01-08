{-# LANGUAGE DeriveFunctor #-}
module Cqrs.EDsl (
  CommandDirective (..),
  RejectionReason,
  CommandTransaction,
  Action (..),
  persistEvent,updateValidationState,getNewEventID,getCurrentTime
 ) where

import Cqrs.Write.Aggregate.Events.Event
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import qualified Data.Time as Time
import Control.Monad.Free
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Write.Aggregate.Events.EventId

data CommandDirective applicationState = Reject RejectionReason | SkipBecauseAlreadyProcessed | Validate (CommandTransaction applicationState ())


data Action applicationState a = PersistEvent Event a
              | UpdateValidationState (ValidationState applicationState) a
              | GetCurrentTime (Time.UTCTime -> a )
              | GetNewEventId (EventId -> a) deriving (Functor)

type CommandTransaction applicationState a = Free (Action applicationState) a

persistEvent :: Event -> CommandTransaction applicationState ()
persistEvent event = Free (PersistEvent event (Pure ()))

updateValidationState :: (ValidationState applicationState) -> CommandTransaction applicationState ()
updateValidationState validationState = Free (UpdateValidationState validationState (Pure ()))

getNewEventID :: CommandTransaction applicationState EventId
getNewEventID = Free (GetNewEventId Pure)

getCurrentTime :: CommandTransaction applicationState Time.UTCTime
getCurrentTime = Free (GetCurrentTime Pure)