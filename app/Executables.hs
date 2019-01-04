{-|
Module      : Executables
Description : GSD Micro Services (Client + Backend)
Copyright   : (c) Nicolas Henin, 2018-2019
License     : Apache-2.0
Stability   : experimental
-}
module Executables where

import Settings

import Gsd.Write.CommandConsumptionStreamer
import Gsd.Write.WebApi
import Gsd.Monitoring.WebStreamingApi
import Gsd.Read.WebStreamingApi
import Gsd.CLI.CLI
import Servant.Client
import Gsd.Clients
--------------------------------------------------------------------------------
-- * GSD Micro Services (Client + Backend)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Client Micro Services
--------------------------------------------------------------------------------

-- | Client Command line : Allow you to use the gsd application
--   (send commands and access to a specific gsd read model )
gsdWriteClientCommandLineInterface :: IO ()
gsdWriteClientCommandLineInterface = Gsd.CLI.CLI.execute Clients{
   writeApiUrl = (BaseUrl Http "localhost" getWriteApiPort ""),
   gsdReadApiUrl = (BaseUrl Http "localhost" getGsdReadStreamingApiPort ""),
   gsdMonitoringApiUrl = (BaseUrl Http "localhost" getGsdMonitoringStreamingApiPort "")}


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
  getEventStoreSettings
  getConnectionType
  getCredentials


--------------------------------------------------------------------------------
-- **  READ Backend Micro Services
--------------------------------------------------------------------------------

-- | Gsd Web Read Api : Web Api readings events and returning an in memory specific read model for gsd
gsdReadStreamingApi :: IO ()
gsdReadStreamingApi = Gsd.Read.WebStreamingApi.execute
  getGsdReadStreamingApiPort
  getEventStoreSettings
  getConnectionType
  getCredentials


-- | Monitoring Streaming Api : Tool to read directly what the Write Channel stored in the EventStore
-- (example of a second useful read model in CQRS applications)
gsdMonitoringStreamingApi :: IO ()
gsdMonitoringStreamingApi = Gsd.Monitoring.WebStreamingApi.execute
  getGsdMonitoringStreamingApiPort
  getEventStoreSettings
  getConnectionType
  getCredentials