{-# LANGUAGE DeriveFunctor #-}
module Cqrs.EDsl (
  CommandDirective (..),
  RejectionReason,
  CommandTransaction,
  Action (..),
  persistEvent,updateSnapshot,getNewEventID,getCurrentTime
 ) where

import Cqrs.Events.Event
import Cqrs.Aggregate.Snapshots.AggregateSnapshot
import qualified Data.Time as Time
import Control.Monad.Free
import Cqrs.Commands.Responses.CommandResponse
import Cqrs.Events.EventId

data CommandDirective = Reject RejectionReason | SkipBecauseAlreadyProcessed | Transact (CommandTransaction ())



data Action a = PersistEvent Event a
              | UpdateSnapshot AggregateSnapshot a
              | GetCurrentTime (Time.UTCTime -> a )
              | GetNewEventId (EventId -> a) deriving (Functor)

type CommandTransaction a = Free Action a

persistEvent :: Event -> CommandTransaction ()
persistEvent event = Free (PersistEvent event (Pure ()))

updateSnapshot :: AggregateSnapshot -> CommandTransaction ()
updateSnapshot aggregateSnapshot = Free (UpdateSnapshot aggregateSnapshot (Pure ()))

getNewEventID :: CommandTransaction EventId
getNewEventID = Free (GetNewEventId Pure)

getCurrentTime :: CommandTransaction Time.UTCTime
getCurrentTime = Free (GetCurrentTime Pure)