{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Executables where

import Settings
import Prelude hiding (read)
import Gsd.Write.CommandConsumptionStreamer
import Gsd.Write.WebApi
import Gsd.Monitoring.Main
import Gsd.Read.WebApi
import Gsd.CLI.CLI
import Servant.Client hiding (manager)
import Gsd.Clients
import DevOps.MicroService.EventStore hiding (getCredentials,getConnectionType,getEventStoreSettings)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Logger.Core
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
  let logger = Logger { loggerId = "[gsd.client.cli]" , executableName = "client.cli" }
  initLogger logger
  manager <- (newManager defaultManagerSettings)
  Gsd.CLI.CLI.execute ClientsSetting {
     write = ClientSetting {manager , url = BaseUrl Http "localhost" getWriteApiPort "", logger},
     read = ClientSetting {manager , url = BaseUrl Http "localhost" getGsdReadStreamingApiPort "",logger},
     monitoring = ClientSetting {manager , url = BaseUrl Http "localhost" getGsdMonitoringStreamingApiPort "",logger}}


--------------------------------------------------------------------------------
-- **  WRITE Backend Micro Services
--------------------------------------------------------------------------------


-- | Gsd Web Write Api : Web Api that receives commands and persist them per Aggregate into the EventStore
gsdWriteApi :: IO ()
gsdWriteApi = Gsd.Write.WebApi.execute
  getWriteApiPort
  getEventStoreSettings
  getConnectionType
  getCredentials

-- | Command consumption streamer :
--  Processes commands stored in the EventStore and produces command responses and events
gsdCommandConsumptionStreamer :: IO ()
gsdCommandConsumptionStreamer = Gsd.Write.CommandConsumptionStreamer.execute
  EventStoreMicroService {
        urlHost = "127.0.0.1",
        port = 1113,
        username = "admin",
        password = "changeit"}


--------------------------------------------------------------------------------
-- **  READ Backend Micro Services
--------------------------------------------------------------------------------

-- | Gsd Web Read Api : Web Api readings events and returning an in memory specific read model for gsd
gsdReadApi :: IO ()
gsdReadApi = Gsd.Read.WebApi.execute
  getGsdReadStreamingApiPort
  getEventStoreSettings
  getConnectionType
  getCredentials


-- | Monitoring Api : Tool to read directly what the Write Channel stored in the EventStore
-- (example of a second useful read model in CQRS applications)
gsdMonitoringApi :: IO ()
gsdMonitoringApi = Gsd.Monitoring.Main.execute
  getGsdMonitoringStreamingApiPort
  EventStoreMicroService {
      urlHost = "127.0.0.1",
      port = 1113,
      username = "admin",
      password = "changeit"}