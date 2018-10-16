{-# LANGUAGE OverloadedStrings #-}
module CommandSourcing.EventStore where
import Conduit
import Control.Monad.Trans.Resource
import qualified Database.EventStore as EventStore

getEventStoreConnection :: ResourceT IO (EventStore.Connection)
getEventStoreConnection = do
    (_, connection) <- allocate (EventStore.connect EventStore.defaultSettings (EventStore.Static "127.0.0.1" 1113))
                         (\connection -> do EventStore.shutdown connection
                                            EventStore.waitTillClosed connection)
    lift $ liftIO $ return connection

getCredentials :: Maybe EventStore.Credentials
getCredentials = Just $ EventStore.credentials "admin" "changeit"