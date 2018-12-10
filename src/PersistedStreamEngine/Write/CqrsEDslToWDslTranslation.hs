module PersistedStreamEngine.Write.CqrsEDslToWDslTranslation where

import Control.Monad.Free
import qualified Cqrs.EDsl as CqrsEDsl
import qualified PersistedStreamEngine.Write.WDsl as WDsl

translate :: CqrsEDsl.CommandTransaction () -> WDsl.WritePersistenceStreamLanguage ()
translate (Pure a)  = return a
translate (Free (CqrsEDsl.PersistEvent event next)) = Free (WDsl.PersistEvent event $ translate next)
translate (Free (CqrsEDsl.UpdateValidationState validationState next))  = Free (WDsl.PersistValidationState validationState $ translate next)
translate (Free (CqrsEDsl.GetCurrentTime fct))  = Free (WDsl.GetCurrentTime $ fmap translate fct)
translate (Free (CqrsEDsl.GetNewEventId fct))  = Free (WDsl.GetNewEventId $ fmap translate fct)