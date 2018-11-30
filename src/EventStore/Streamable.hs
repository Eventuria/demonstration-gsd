{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module EventStore.Streamable where

import EventStore.Read.PersistedItem
import Control.Monad.IO.Class
import Streamly
import Data.Aeson

class (FromJSON item,
       Monad m,
       IsStream stream,
       MonadIO (stream m),
       MonadAsync m,
       Semigroup (stream m (Persisted item))) => Streamable m stream item

instance FromJSON item =>  Streamable IO SerialT item
instance FromJSON item =>  Streamable IO ParallelT item