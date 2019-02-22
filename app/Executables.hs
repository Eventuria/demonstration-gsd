{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Executables where


import Prelude hiding (read)

import Eventuria.GSD.Write.Flow.Sourcer.Server.Server
import Eventuria.GSD.Monitoring.API.Server.Server
import Eventuria.GSD.Read.API.Server.Server
import Eventuria.GSD.CLI.CLI
import Eventuria.Commons.Network.Core

import Eventuria.Commons.Logger.Core
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Settings as EventStoreClient
import Eventuria.GSD.Write.Flow.Sourcer.Server.Settings
import qualified Eventuria.GSD.CLI.Settings as CLI

import qualified Eventuria.GSD.Write.Flow.Sourcer.Client.Settings as Write.Client
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.API.HealthCheck.Client.Settings as Write.Command.Consumer.Client
import qualified Eventuria.GSD.Read.API.Client.Settings as Read.Client
import qualified Eventuria.GSD.Monitoring.API.Client.Settings as Monitoring.Client

import qualified Eventuria.GSD.Write.Flow.Sourcer.Server.Settings as Write.Server

import qualified Eventuria.GSD.Read.API.Server.Settings as Read.Server
import qualified Eventuria.GSD.Monitoring.API.Server.Settings as Monitoring.Server
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.Settings as Command.Consumer
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.Consumer as Command.Consumer
--------------------------------------------------------------------------------
-- * GSD Micro Services (Client + Backend)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Client Micro Services
--------------------------------------------------------------------------------

-- | Client Command line : Allow you to use the gsd application
--   (send commands and access to a specific gsd read model )
gsdWriteClientCommandLineInterface :: IO ()
gsdWriteClientCommandLineInterface = Eventuria.GSD.CLI.CLI.execute CLI.Settings {
                                                           loggerId = "[gsd.cli]",
                                                           writeClientSettings = Write.Client.Settings {
                                                                            loggerId = "[gsd.cli/write.client]" , 
                                                                            url = URL { host = "localhost", 
                                                                                        port = 3000,
                                                                                        path = ""}},
                                                           writeCommandConsumerClientSettings = Write.Command.Consumer.Client.Settings {
                                                                            loggerId = "[gsd.cli/write.command.consumer.client]" ,
                                                                            url = URL { host = "localhost",
                                                                                        port = 3001,
                                                                                        path = ""}},
                                                           readClientSettings = Read.Client.Settings {
                                                                            loggerId = "[gsd.cli/read.client]" , 
                                                                            url = URL { host = "localhost", 
                                                                                        port = 3002,
                                                                                        path = ""}},
                                                           monitoringClientSettings = Monitoring.Client.Settings {
                                                                            loggerId = "[gsd.cli/monitoring.client]" , 
                                                                            url = URL { host = "localhost", 
                                                                                        port = 3003,
                                                                                        path = ""}}}


--------------------------------------------------------------------------------
-- **  WRITE Backend Micro Services
--------------------------------------------------------------------------------


-- | Gsd Web Write Api : Web Api that receives commands and persist them per Aggregate into the EventStore
gsdWriteApi :: IO ()
gsdWriteApi = Eventuria.GSD.Write.Flow.Sourcer.Server.Server.start
                Write.Server.Settings { serviceLoggerId = "[gsd.write.server]",
                                        healthCheckLoggerId = "[gsd.write.server/healthcheck]",
                                        port = 3000,
                                        eventStoreClientSettings = getEventStoreSettings
                                                                       "[gsd.write.server/event.store.client]"}

-- | Command consumption streamer :
--  Processes commands stored in the EventStore and produces command responses and events
gsdCommandConsumptionStreamer :: IO ()
gsdCommandConsumptionStreamer = Command.Consumer.start
                                  Command.Consumer.Settings {
                                    serviceLoggerId = "[gsd.write.command.consummer]",
                                    healthCheckLoggerId = "[gsd.write.command.consummer/healthcheck]",
                                    port = 3001,
                                    eventStoreClientSettings = getEventStoreSettings
                                                                  "[gsd.write.command.consummer/event.store.client]"}

  
--------------------------------------------------------------------------------
-- **  READ Backend Micro Services
--------------------------------------------------------------------------------

-- | Gsd Web Read Api : Web Api readings events and returning an in memory specific read model for gsd
gsdReadApi :: IO ()
gsdReadApi = Eventuria.GSD.Read.API.Server.Server.start
                    Read.Server.Settings {
                      serviceLoggerId = "[gsd.read.server]",
                      healthCheckLoggerId = "[gsd.read.server/healthcheck]",
                      port = 3002, 
                      eventStoreClientSettings = getEventStoreSettings "[gsd.read.server/event.store.client]"}


-- | Monitoring Api : Tool to read directly what the Write Channel stored in the EventStore
-- (example of a second useful read model in CQRS applications)
gsdMonitoringApi :: IO ()
gsdMonitoringApi = Eventuria.GSD.Monitoring.API.Server.Server.start
                    Monitoring.Server.Settings {
                      serviceLoggerId = "[gsd.monitoring.server]",
                      healthCheckLoggerId = "[gsd.monitoring.server/healthcheck]",
                      port = 3003,
                      eventStoreClientSettings = getEventStoreSettings "[gsd.monitoring.server/event.store.client]"}

getEventStoreSettings :: LoggerId -> EventStoreClient.Settings
getEventStoreSettings loggerId = EventStoreClient.Settings {
                                                    urlHost = "127.0.0.1",
                                                    port = 1113,
                                                    path = "",
                                                    username = "admin",
                                                    password = "changeit",
                                                    loggerId }

