{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module PersistedStreamEngine.Interface.Streamable where

import PersistedStreamEngine.Interface.PersistedItem
import Control.Monad.IO.Class
import Streamly
import Data.Aeson

class (FromJSON item,
       Monad monad,
       IsStream stream,
       MonadIO (stream monad),
       MonadAsync monad,
       Semigroup (stream monad (Persisted item))) => Streamable stream monad item

instance FromJSON item =>  Streamable SerialT IO item
instance FromJSON item =>  Streamable ParallelT IO  item