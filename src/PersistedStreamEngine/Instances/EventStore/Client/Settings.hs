module PersistedStreamEngine.Instances.EventStore.Client.Settings where

import Logger.Core
import Network.Core
import Data.ByteString

data Settings = Settings {
                                    loggerId :: LoggerId,
                                    urlHost :: URLHost,
                                    port :: URLPort,
                                    path :: URLPath,
                                    username :: ByteString,
                                    password :: ByteString}

