{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Gsd.Write.Client (sendCommand) where


import Data.Proxy
import Servant.Client
import PersistedStreamEngine.Interface.Write.PersistenceResult

import Gsd.Write.Commands.Command
import Gsd.Write.WebApi

sendCommand :: GsdCommand -> ClientM PersistenceResult

api :: Proxy GsdWriteApi
api = Proxy

sendCommand = client api