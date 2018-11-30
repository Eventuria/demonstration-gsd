module Cqrs.EventStore.Translation where

import Control.Monad.Free
import qualified Cqrs.EDsl as CqrsEDsl
import qualified Cqrs.EventStore.Write.WDsl as EventStoreDsl

translate :: CqrsEDsl.CommandTransaction () -> EventStoreDsl.WriteEventStoreLanguage ()
translate (Pure a)  = return a
translate (Free (CqrsEDsl.PersistEvent event next)) = Free (EventStoreDsl.PersistEvent event $ translate next)
translate (Free (CqrsEDsl.UpdateValidationState validationState next))  = Free (EventStoreDsl.PersistValidationState validationState $ translate next)
translate (Free (CqrsEDsl.GetCurrentTime fct))  = Free (EventStoreDsl.GetCurrentTime $ fmap translate fct)
translate (Free (CqrsEDsl.GetNewEventId fct))  = Free (EventStoreDsl.GetNewEventId $ fmap translate fct)