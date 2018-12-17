module Executables where

import Settings

import Gsd.Write.CommandConsumptionStreamer
import Gsd.Write.WebApi
import Gsd.Read.Monitoring.WebFetchingApi
import Gsd.Read.Monitoring.WebStreamingApi

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

gsdMonitoringFetchingApi :: IO ()
gsdMonitoringFetchingApi = Gsd.Read.Monitoring.WebFetchingApi.execute
  getGsdMonitoringFetchingApiPort
  getEventStoreSettings
  getConnectionType
  getCredentials

gsdMonitoringStreamingApi :: IO ()
gsdMonitoringStreamingApi = Gsd.Read.Monitoring.WebStreamingApi.execute
  getGsdMonitoringStreamingApiPort
  getEventStoreSettings
  getConnectionType
  getCredentials