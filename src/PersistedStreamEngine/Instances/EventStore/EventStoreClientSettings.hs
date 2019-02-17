module PersistedStreamEngine.Instances.EventStore.EventStoreClientSettings where

import Logger.Core
import Network.Core
import Data.ByteString

data EventStoreClientSettings = EventStoreClientSettings {
                                    loggerId :: LoggerId,
                                    urlHost :: URLHost,
                                    port :: URLPort,
                                    path :: URLPath,
                                    username :: ByteString,
                                    password :: ByteString}

