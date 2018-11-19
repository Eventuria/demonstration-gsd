{-# LANGUAGE DeriveFunctor #-}
module Cqrs.EDsl (
  CommandDirective (..),
  RejectionReason,
  CommandTransaction,
  Action (..),
  persistEvent,updateSnapshot,getNewEventID,getCurrentTime
 ) where

import Cqrs.PersistedCommand
import Cqrs.Snapshot
import Cqrs.Events
import Cqrs.Core
import qualified Data.Time as Time
import Control.Monad.Free
import Cqrs.CommandResponse

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