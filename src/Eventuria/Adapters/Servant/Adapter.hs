{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.Adapters.Servant.Adapter where

import Eventuria.Commons.Network.Core
import Servant.Client

toBaseUrl :: URL -> BaseUrl
toBaseUrl URL {host, port, path} = BaseUrl Http host port path

toUrl :: BaseUrl -> URL
toUrl    BaseUrl {baseUrlScheme
                , baseUrlHost = host
                , baseUrlPort = port
                , baseUrlPath = path} = URL {..}