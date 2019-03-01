{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Eventuria.Adapters.Streamly.Safe where

import           Control.Concurrent (ThreadId)
import           Control.Exception

import           Data.Either
import           Data.Function ((&))

import qualified Streamly.Prelude as S
import           Streamly

map :: (IsStream t, Monad m) => (a -> b) -> t m ( Either SomeException a) -> t m (Either SomeException b)
map transformation stream = stream & S.map (\safeResponse -> case safeResponse of
        Right a -> Right $ transformation a
        Left error -> Left error )

mapM :: (IsStream t, MonadAsync m) => (a -> m (Either SomeException b)) -> t m (Either SomeException a) -> t m (Either SomeException b)
mapM transformation stream = stream & S.mapM (\safeResponse -> case safeResponse of
    Right a -> transformation a
    Left error -> return $ Left error )

filter :: (IsStream t, Monad m) => (a -> Bool) -> t m (Either SomeException a) -> t m (Either SomeException a)
filter filtering stream = stream & S.filter (\safeResponse -> case safeResponse of
    Right a -> filtering a
    Left left -> True )


foldx :: forall x m a b. Monad m => (x -> a ->  x)
                     -> x
                     -> (x -> b)
                     -> SerialT m (Either SomeException a) -> m (Either SomeException b)
foldx folding element extraction stream =
  S.foldx
    (\safeResponseA safeResponseB ->
            case (safeResponseA, safeResponseB) of
              (Left error , _ ) -> Left error
              (_ , Left error ) -> Left error
              (Right a , Right b) ->
                  Right $ folding a b)
    (Right element)
    (\safeResponse -> fmap extraction safeResponse)
    stream

fromList :: (Monad m, IsStream t) => Either SomeException [a] -> t m (Either SomeException a)
fromList safeResponse = case safeResponse of
                    Right items -> S.fromList $ Right <$> items
                    Left error -> S.yield $ Left error

yield :: IsStream t => a -> t m (Either SomeException a)
yield element = S.yield $ Right element


runStreamOnIOAndThrowFailureTo :: ThreadId ->
                                SerialT IO ( Either SomeException a) -> IO ()
runStreamOnIOAndThrowFailureTo threadId stream = runStream $
  stream
    & S.mapM (\safeResponse -> case safeResponse of
            Right a -> return $ Right $ a
            Left error -> do
              throwTo threadId error
              return $ Left error  )


-- Bunch of bad smells
toList :: SerialT IO (Either SomeException a) -> IO (Either SomeException [a])
toList stream =  do
    items <- catch
              (S.toList stream)
              (\error @ SomeException {} -> return $ [Left $ toException error])
    case (reverse items) of
      [] -> return $ Right []
      (Right lastItem :xs )  -> return $ Right $ removeSafeResponseLayer items
      (Left error : xs) -> return $ Left error

  where
    removeSafeResponseLayer :: [Either SomeException a] -> [a]
    removeSafeResponseLayer items = (fromRight blowUp) <$> items

    blowUp :: a
    blowUp = error "streams should only terminate with an eventual Left, a Left should not been introduced in the middle of a stream"
