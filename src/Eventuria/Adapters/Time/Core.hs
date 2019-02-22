module Eventuria.Adapters.Time.Core where

type Seconds = Int
type MilliSeconds = Int

getInMsFromSeconds :: Seconds -> MilliSeconds
getInMsFromSeconds = (*) 1000000