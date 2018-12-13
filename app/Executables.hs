module Executables where

import Settings

import Gsd.Write.CommandConsumptionStreamer
import Gsd.Write.WebApi
import Gsd.Read.Monitoring.WebApi

gsdCommandConsumptionStreamer :: IO ()
gsdCommandConsumptionStreamer = Gsd.Write.CommandConsumptionStreamer.execute
  getEventStoreSettings
  getConnectionType
  getCredentials

gsdWriteApi :: IO ()
gsdWriteApi = Gsd.Write.WebApi.execute
  getWriteApiPort
  getEventStoreSettings
  getConnectionType
  getCredentials

gsdMonitoringApi :: IO ()
gsdMonitoringApi = Gsd.Read.Monitoring.WebApi.execute
  getGsdMonitoringApiPort
  getEventStoreSettings
  getConnectionType
  getCredentials
