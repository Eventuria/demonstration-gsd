{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Write.Transactions where

import qualified Data.Text    as Text
import qualified Data.UUID.V4 as Uuid

import           Control.Concurrent.Async (wait)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Exception

import qualified Database.EventStore as EventStore

import           Eventuria.Commons.Logger.Core

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writable
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.PersistenceResult
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies


data EventStoreStreamUnderTransaction item = EventStoreStreamUnderTransaction {
                                           stream :: EventStoreStream item,
                                           transaction :: EventStore.Transaction}

startTransaction :: EventStoreStream item -> IO (Either SomeException (EventStoreStreamUnderTransaction item))
startTransaction stream @ EventStoreStream {
                                     clientDependencies = Dependencies { logger, credentials, connection },
                                     streamName } =
  catch
    (EventStore.startTransaction
        connection
        streamName
        EventStore.anyVersion
        (Just credentials) >>=
     wait >>=
     (\transaction -> (return . Right) EventStoreStreamUnderTransaction {stream,transaction}))
    (\e @ SomeException {}  -> do
        liftIO $ logInfo logger $ "[start.transaction] exception raised "  ++ show e
        return $ Left e)

endTransaction :: EventStoreStreamUnderTransaction item -> IO (Either SomeException PersistenceResult)
endTransaction EventStoreStreamUnderTransaction {
                         transaction,
                         stream = EventStoreStream {clientDependencies = Dependencies {logger,credentials}}} =
  catch
    (do
      writeResult <- EventStore.transactionCommit
        transaction
        (Just credentials) >>= wait
      return $ Right $ PersistenceResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult)
    (\e @ SomeException {}  -> do
            liftIO $ logInfo logger $ "[end.transaction] exception raised "  ++ show e
            return $ Left e)

persist :: Writable item =>  EventStoreStreamUnderTransaction item -> item -> IO (Either SomeException ())
persist EventStoreStreamUnderTransaction {
          transaction,
          stream = EventStoreStream {clientDependencies = Dependencies {logger,credentials}}}
        itemToPersist =
    catch
    (do
        eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom
        let eventType               = EventStore.UserDefined $ Text.pack $ getItemName itemToPersist
            eventData               = EventStore.withJson itemToPersist
            eventInEventStoreDomain = EventStore.createEvent eventType (Just eventIdInEventStoreDomain) eventData

        liftIO $ EventStore.transactionWrite
                transaction
                [eventInEventStoreDomain]
                (Just credentials) >>= wait >>= return . Right )
    (\e @ SomeException {}  -> do
        liftIO $ logInfo logger $ "[persist] exception raised "  ++ show e
        return $ Left e)