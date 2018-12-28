{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.Read.GenericRead (streamWorkspaces) where


import Data.Function ((&))
import qualified Streamly.Prelude as S

import PersistedStreamEngine.Interface.Streamable
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.PersistedItem


import PersistedStreamEngine.Interface.Read.Reading
import Gsd.Read.Workspace
import Gsd.Write.Core

streamWorkspaces :: Streamable stream monad WorkspaceId => AggregateIdStream persistedStream -> Streaming persistedStream -> stream monad (Persisted Workspace)
streamWorkspaces aggregateIdStream Streaming {streamAll} =
    (streamAll $ aggregateIdStream)
      & S.map (\PersistedItem { offset = offset, item = workspaceId} ->
                PersistedItem { offset = offset, item = Workspace {workspaceId , name = "todo : Add a name to aggregate"}} )


