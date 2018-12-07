module Cqrs.PersistedStream.Write.Writable where

import Data.Aeson

class ToJSON item => Writable item where
  getItemName :: item -> String
