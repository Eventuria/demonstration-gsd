module Executables where

import Settings

import Gsd.Write.CommandConsumptionStreamer
import Gsd.Write.Api
import Gsd.Read.Monitoring.Api

gsdCommandConsumptionStreamer :: IO ()
gsdCommandConsumptionStreamer = Gsd.Write.CommandConsumptionStreamer.execute
  getEventStoreSettings
  getConnectionType
  getCredentials

gsdWriteApi :: IO ()
gsdWriteApi = Gsd.Write.Api.execute
  getWriteApiPort
  getEventStoreSettings
  getConnectionType
  getCredentials

gsdMonitoringApi :: IO ()
gsdMonitoringApi = Gsd.Read.Monitoring.Api.execute
  getGsdMonitoringApiPort
  getEventStoreSettings
  getConnectionType
  getCredentials
