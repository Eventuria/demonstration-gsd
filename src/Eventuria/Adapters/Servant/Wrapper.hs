{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Eventuria.Adapters.Servant.Wrapper where

import Servant
import qualified Pipes as P


type PipeStream item = P.Producer (item) IO ()
type ServantApplication api serverDependencies  = Proxy api -> ServantServer api serverDependencies -> serverDependencies -> Application
type ServantServer api serverDependencies = serverDependencies -> Server api

application :: HasServer api '[] => ServantApplication api dependencies
application proxy server dependencies = serve
                                          proxy
                                        $ server dependencies

proxy :: Proxy api
proxy = Proxy


