module Gsd.Gsd (requestCommand,streamCommands,streamWorkspaceIds) where


import Gsd.Commands
import Gsd.Core
import qualified Cqrs.Cqrs as Cqrs
import Cqrs.Streams
import Cqrs.EventStore.Context
import Streamly
import Cqrs.EventStore.PersistedItem
import Cqrs.Aggregate.Commands.Command
import Cqrs.EventStore.Streaming
import Cqrs.Aggregate.Commands.CommandStream
import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import qualified Streamly.Prelude as S
import Data.Maybe
import Cqrs.Aggregate.Ids.AggregateIdStream

requestCommand :: EventStoreContext -> GsdCommand -> IO (Either PersistenceFailure PersistResult)
requestCommand eventStoreContext gsdCommand = Cqrs.persistCommands eventStoreContext $ toCommand gsdCommand



streamWorkspaceIds :: (IsStream stream , MonadIO (stream IO), Semigroup (stream IO (Persisted WorkspaceId)))=>
                                            EventStoreContext ->
                                            stream IO (Persisted WorkspaceId)
streamWorkspaceIds eventStoreContext = streamAll $ getAggregateIdStream eventStoreContext


streamCommands ::  (IsStream stream , MonadIO (stream IO), Semigroup (stream IO (Persisted Command)))=>
                         EventStoreContext ->
                         WorkspaceId ->
                         stream IO (Persisted GsdCommand)
streamCommands eventStoreContext workspaceId = do
  let commandStream =  (getCommandStream eventStoreContext workspaceId)
  (streamAll commandStream & S.map (\PersistedItem { offset = offset, item = cqrsCommand} -> PersistedItem { offset = offset, item = fromJust $ fromCommand $ cqrsCommand}))