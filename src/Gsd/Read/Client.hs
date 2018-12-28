{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Gsd.Read.Client (streamWorkspaces) where

import Data.Proxy

import Gsd.Read.Workspace

import Streamly.Adapters
import Gsd.Read.WebStreamingApi
import qualified Servant.Client.Streaming as S

import PersistedStreamEngine.Interface.PersistedItem

import Servant.Pipes ()

import Streamly

gsdReadStreamingApi :: Proxy StreamWorkspaces
gsdReadStreamingApi = Proxy

streamWorkspaces :: IsStream stream => S.ClientM (stream IO (Persisted Workspace) )
streamWorkspaces = fromPipes <$> S.client gsdReadStreamingApi