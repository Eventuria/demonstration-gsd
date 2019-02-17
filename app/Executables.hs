{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Executables where


import Prelude hiding (read)

import Gsd.Write.API.Server.Server
import Gsd.Monitoring.API.Server.Server
import Gsd.Read.API.Server.Server
import Gsd.CLI.CLI
import Servant.Client hiding (manager)
import Gsd.Clients
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Logger.Core
import PersistedStreamEngine.Instances.EventStore.EventStoreClientSettings
import Gsd.Write.API.Server.Settings
import qualified Gsd.Write.API.Server.Settings as WriteServer
import qualified Gsd.Read.API.Server.ServerSettings as ReadServer
import qualified Gsd.Monitoring.API.Server.Settings as MonitoringServer
import qualified Gsd.Write.Commands.Consumer.CommandConsumerSettings as CommandConsumer
import qualified Gsd.Write.Commands.Consumer.CommandConsumer as CommandConsumer
--------------------------------------------------------------------------------
-- * GSD Micro Services (Client + Backend)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Client Micro Services
--------------------------------------------------------------------------------

-- | Client Command line : Allow you to use the gsd application
--   (send commands and access to a specific gsd read model )
gsdWriteClientCommandLineInterface :: IO ()
gsdWriteClientCommandLineInterface = do
  writeLogger <- getLogger "[gsd.cli.monitoring.client]"
  readLogger <- getLogger "[gsd.cli.monitoring.client]"
  monitoringLogger <- getLogger "[gsd.cli.monitoring.client]"
  manager <- (newManager defaultManagerSettings)
  Gsd.CLI.CLI.execute ClientsSetting {
     write = ClientSetting {manager , url = BaseUrl Http "localhost" 3000 "", logger = writeLogger },
     read = ClientSetting {manager , url = BaseUrl Http "localhost" 3001 "",logger = readLogger},
     monitoring = ClientSetting {manager , url = BaseUrl Http "localhost" 3002 "",logger = monitoringLogger}}


--------------------------------------------------------------------------------
-- **  WRITE Backend Micro Services
--------------------------------------------------------------------------------


-- | Gsd Web Write Api : Web Api that receives commands and persist them per Aggregate into the EventStore
gsdWriteApi :: IO ()
gsdWriteApi = getLogger "[gsd.write.server]" >>= (\logger -> Gsd.Write.API.Server.Server.start WriteServer.ServerSettings { port = 3000, eventStoreClientSettings = getEventStoreSettingsToChangeName "[write.server/event.store.client]",logger})

-- | Command consumption streamer :
--  Processes commands stored in the EventStore and produces command responses and events
gsdCommandConsumptionStreamer :: IO ()
gsdCommandConsumptionStreamer = getLogger "[gsd.Write.CommandConsummer]" >>= (\logger -> CommandConsumer.start CommandConsumer.CommandConsumerSettings { eventStoreClientSettings = getEventStoreSettingsToChangeName "[command.consummer/event.store.client]", logger})

  
--------------------------------------------------------------------------------
-- **  READ Backend Micro Services
--------------------------------------------------------------------------------

-- | Gsd Web Read Api : Web Api readings events and returning an in memory specific read model for gsd
gsdReadApi :: IO ()
gsdReadApi = getLogger "[gsd.read.server]" >>= (\logger -> Gsd.Read.API.Server.Server.start
                    ReadServer.ServerSettings {
                      port = 3001, 
                      eventStoreClientSettings = getEventStoreSettingsToChangeName "[read.server/event.store.client]", logger})


-- | Monitoring Api : Tool to read directly what the Write Channel stored in the EventStore
-- (example of a second useful read model in CQRS applications)
gsdMonitoringApi :: IO ()
gsdMonitoringApi = getLogger "[gsd.monitoring.server]" >>= (\logger -> Gsd.Monitoring.API.Server.Server.start
                    MonitoringServer.ServerSettings {
                      port = 3002,
                      eventStoreClientSettings = getEventStoreSettingsToChangeName "[monitoring.server/event.store.client]", logger})

getEventStoreSettingsToChangeName :: LoggerId -> EventStoreClientSettings
getEventStoreSettingsToChangeName loggerId = EventStoreClientSettings {
                                                    urlHost = "127.0.0.1",
                                                    port = 1113,
                                                    path = "",
                                                    username = "admin",
                                                    password = "changeit",
                                                    loggerId }

