{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Executables where


import Prelude hiding (read)

import Gsd.Write.API.Server.Server
import Gsd.Monitoring.API.Server.Server
import Gsd.Read.API.Server.Server
import Gsd.CLI.CLI
import Network.Core

import Logger.Core
import PersistedStreamEngine.Instances.EventStore.EventStoreClientSettings
import Gsd.Write.API.Server.Settings
import qualified Gsd.CLI.Settings as CLI
import qualified Gsd.Read.API.Client.Settings as Read.Client
import qualified Gsd.Write.API.Client.Settings as Write.Client
import qualified Gsd.Monitoring.API.Client.Settings as Monitoring.Client

import qualified Gsd.Write.API.Server.Settings as Write.Server
import qualified Gsd.Read.API.Server.Settings as Read.Server
import qualified Gsd.Monitoring.API.Server.Settings as Monitoring.Server
import qualified Gsd.Write.Command.Consumer.Settings as Command.Consumer
import qualified Gsd.Write.Command.Consumer.CommandConsumer as Command.Consumer
--------------------------------------------------------------------------------
-- * GSD Micro Services (Client + Backend)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Client Micro Services
--------------------------------------------------------------------------------

-- | Client Command line : Allow you to use the gsd application
--   (send commands and access to a specific gsd read model )
gsdWriteClientCommandLineInterface :: IO ()
gsdWriteClientCommandLineInterface = Gsd.CLI.CLI.execute CLI.Settings {
                                                           loggerId = "[gsd.cli]",
                                                           writeClientSettings = Write.Client.Settings {
                                                                            loggerId = "[gsd.cli/write.client]" , 
                                                                            url = URL { host = "localhost", 
                                                                                        port = 3000,
                                                                                        path = ""}},
                                                           readClientSettings = Read.Client.Settings {
                                                                            loggerId = "[gsd.cli/read.client]" , 
                                                                            url = URL { host = "localhost", 
                                                                                        port = 3001,
                                                                                        path = ""}},
                                                           monitoringClientSettings = Monitoring.Client.Settings {
                                                                            loggerId = "[gsd.cli/monitoring.client]" , 
                                                                            url = URL { host = "localhost", 
                                                                                        port = 3002,
                                                                                        path = ""}}}


--------------------------------------------------------------------------------
-- **  WRITE Backend Micro Services
--------------------------------------------------------------------------------


-- | Gsd Web Write Api : Web Api that receives commands and persist them per Aggregate into the EventStore
gsdWriteApi :: IO ()
gsdWriteApi = Gsd.Write.API.Server.Server.start
                Write.Server.Settings { loggerId = "[write.server]",
                                        port = 3000,
                                        eventStoreClientSettings = getEventStoreSettings
                                                                       "[write.server/event.store.client]"}

-- | Command consumption streamer :
--  Processes commands stored in the EventStore and produces command responses and events
gsdCommandConsumptionStreamer :: IO ()
gsdCommandConsumptionStreamer = Command.Consumer.start
                                  Command.Consumer.Settings {
                                    loggerId = "[gsd.write.command.consummer]",
                                    eventStoreClientSettings = getEventStoreSettings
                                                                  "[gsd.write.command.consummer/event.store.client]"}

  
--------------------------------------------------------------------------------
-- **  READ Backend Micro Services
--------------------------------------------------------------------------------

-- | Gsd Web Read Api : Web Api readings events and returning an in memory specific read model for gsd
gsdReadApi :: IO ()
gsdReadApi = Gsd.Read.API.Server.Server.start
                    Read.Server.Settings {
                      loggerId = "[gsd.read.server]",
                      port = 3001, 
                      eventStoreClientSettings = getEventStoreSettings "[read.server/event.store.client]"}


-- | Monitoring Api : Tool to read directly what the Write Channel stored in the EventStore
-- (example of a second useful read model in CQRS applications)
gsdMonitoringApi :: IO ()
gsdMonitoringApi = Gsd.Monitoring.API.Server.Server.start
                    Monitoring.Server.Settings {
                      loggerId = "[gsd.monitoring.server]",
                      port = 3002,
                      eventStoreClientSettings = getEventStoreSettings "[monitoring.server/event.store.client]"}

getEventStoreSettings :: LoggerId -> EventStoreClientSettings
getEventStoreSettings loggerId = EventStoreClientSettings {
                                                    urlHost = "127.0.0.1",
                                                    port = 1113,
                                                    path = "",
                                                    username = "admin",
                                                    password = "changeit",
                                                    loggerId }

