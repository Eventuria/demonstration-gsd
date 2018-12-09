module Cqrs.PersistedStreamEngine.Write.Writable where

import Data.Aeson

class ToJSON item => Writable item where
  getItemName :: item -> String
