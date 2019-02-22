module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Settings where

import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core
import Data.ByteString

data Settings = Settings {
                                    loggerId :: LoggerId,
                                    urlHost :: URLHost,
                                    port :: URLPort,
                                    path :: URLPath,
                                    username :: ByteString,
                                    password :: ByteString}

