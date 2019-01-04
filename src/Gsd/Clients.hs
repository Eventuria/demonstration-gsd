module Gsd.Clients where

import Servant.Client (BaseUrl)

data Clients = Clients { writeApiUrl :: BaseUrl ,
                         gsdReadApiUrl :: BaseUrl,
                         gsdMonitoringApiUrl :: BaseUrl}