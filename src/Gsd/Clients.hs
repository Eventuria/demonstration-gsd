module Gsd.Clients where

import Servant.Client (BaseUrl)
import Network.HTTP.Client (Manager)
import Logger.Core
data ClientsSetting = ClientsSetting { write :: ClientSetting ,
                         read :: ClientSetting,
                         monitoring :: ClientSetting}


data ClientSetting = ClientSetting {url :: BaseUrl, manager :: Manager, logger :: Logger }