{-# LANGUAGE FlexibleContexts #-}
module Streamly.Adapters where

import Streamly
import qualified Streamly.Prelude as S
import qualified Pipes as P
import qualified Pipes.Prelude as P

import qualified Streaming as SG
import qualified Streaming.Prelude as SG


-- | pipes to streamly
fromPipes :: (IsStream t, MonadAsync m) => P.Producer a m r -> t m a
fromPipes = S.unfoldrM unconsP
    where
    -- Adapt P.next to return a Maybe instead of Either
    unconsP p = P.next p >>= either (\_ -> return Nothing) (return . Just)

-- | streamly to pipes
toPipes :: Monad m => SerialT m a -> P.Producer a m ()
toPipes = P.unfoldr unconsS
    where
    -- Adapt S.uncons to return an Either instead of Maybe
    unconsS s = S.uncons s >>= maybe (return $ Left ()) (return . Right)


-- | streaming to streamly
fromStreaming :: (IsStream t, MonadAsync m) => SG.Stream (SG.Of a) m r -> t m a
fromStreaming = S.unfoldrM SG.uncons

-- | streamly to streaming
toStreaming :: Monad m => SerialT m a -> SG.Stream (SG.Of a) m ()
toStreaming = SG.unfoldr unconsS
    where
    -- Adapt S.uncons to return an Either instead of Maybe
    unconsS s = S.uncons s >>= maybe (return $ Left ()) (return . Right)