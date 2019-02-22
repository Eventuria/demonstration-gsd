{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Executables where


import Prelude hiding (read)
import Eventuria.Commons.Network.Core
import Eventuria.Commons.Logger.Core

import qualified Eventuria.GSD.CLI.CLI as CLI
import qualified Eventuria.GSD.CLI.Settings as CLI

import qualified Eventuria.GSD.Write.CommandSourcer.Server.Server   as Command.Sourcer
import qualified Eventuria.GSD.Write.CommandSourcer.Server.Settings as Command.Sourcer
import qualified Eventuria.GSD.Write.CommandSourcer.Client.Settings as Command.Sourcer.Client


import qualified Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Client.Settings as Command.Consumer.Client
import qualified Eventuria.GSD.Write.CommandConsumer.Settings as Command.Consumer
import qualified Eventuria.GSD.Write.CommandConsumer.Consumer as Command.Consumer

import qualified Eventuria.GSD.Read.API.Server.Server   as Read
import qualified Eventuria.GSD.Read.API.Server.Settings as Read
import qualified Eventuria.GSD.Read.API.Client.Settings as Read.Client

import qualified Eventuria.GSD.Monitoring.API.Server.Server   as Monitoring
import qualified Eventuria.GSD.Monitoring.API.Server.Settings as Monitoring
import qualified Eventuria.GSD.Monitoring.API.Client.Settings as Monitoring.Client

import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Settings as EventStoreClient


cli :: IO ()
cli =
  CLI.execute
    CLI.Settings {
     loggerId =                  "[eventuria.gsd.cli]",
     writeClientSettings = Command.Sourcer.Client.Settings {
                      loggerId = "[eventuria.gsd.cli/command.sourcer.client]" ,
                      url = URL { host = "localhost",
                                  port = 3000,
                                  path = ""}},
     writeCommandConsumerClientSettings = Command.Consumer.Client.Settings {
                      loggerId = "[eventuria.gsd.cli/command.consumer.client]" ,
                      url = URL { host = "localhost",
                                  port = 3001,
                                  path = ""}},
     readClientSettings = Read.Client.Settings {
                      loggerId = "[eventuria.gsd.cli/read.client]" ,
                      url = URL { host = "localhost",
                                  port = 3002,
                                  path = ""}},
     monitoringClientSettings = Monitoring.Client.Settings {
                      loggerId = "[eventuria.gsd.cli/monitoring.client]" ,
                      url = URL { host = "localhost",
                                  port = 3003,
                                  path = ""}}}


commandSourcer :: IO ()
commandSourcer =
  Command.Sourcer.start
    Command.Sourcer.Settings {
        serviceLoggerId =                                "[eventuria.gsd.command.sourcer]",
        healthCheckLoggerId =                            "[eventuria.gsd.command.sourcer/healthcheck]",
        eventStoreClientSettings = getEventStoreSettings "[eventuria.gsd.command.sourcer/event.store.client]",
        port = 3000}


commandConsumer :: IO ()
commandConsumer =
  Command.Consumer.start
    Command.Consumer.Settings {
      serviceLoggerId =                                "[eventuria.gsd.command.consummer]",
      healthCheckLoggerId =                            "[eventuria.gsd.command.consummer/healthcheck]",
      eventStoreClientSettings = getEventStoreSettings "[eventuria.gsd.write.command.consummer/event.store.client]",
      port = 3001}


read :: IO ()
read =
  Read.start
    Read.Settings {
      serviceLoggerId =                                "[eventuria.gsd.read.server]",
      healthCheckLoggerId =                            "[eventuria.gsd.read.server/healthcheck]",
      eventStoreClientSettings = getEventStoreSettings "[eventuria.gsd.read.server/event.store.client]",
      port = 3002}

monitoring:: IO ()
monitoring =
  Monitoring.start
    Monitoring.Settings {
      serviceLoggerId =                                "[eventuria.gsd.monitoring.server]",
      healthCheckLoggerId =                            "[eventuria.gsd.monitoring.server/healthcheck]",
      eventStoreClientSettings = getEventStoreSettings "[eventuria.gsd.monitoring.server/event.store.client]",
      port = 3003}


getEventStoreSettings :: LoggerId -> EventStoreClient.Settings
getEventStoreSettings loggerId = EventStoreClient.Settings {
                                                    urlHost = "127.0.0.1",
                                                    port = 1113,
                                                    path = "",
                                                    username = "admin",
                                                    password = "changeit",
                                                    loggerId }

