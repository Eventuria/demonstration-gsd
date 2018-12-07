module Cqrs.PersistedStream.Write.PersistenceResult where

data PersistenceFailure = ItemAlreadyPersisted
data PersistResult = PersistResult {writeNextVersion :: Integer}
