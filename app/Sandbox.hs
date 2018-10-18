module Sandbox where


import qualified CommandSourcing.WorkspaceStream as WorkspaceStream
import qualified CommandSourcing.CommandStream as CommandStream
import CommandSourcing.Core
import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import System.Random
import Data.UUID
import qualified Database.EventStore as EventStore
import Control.Exception
import System.Log.Logger
import CommandSourcing.Commands
import Data.Semigroup (Semigroup(..))

main :: IO ()
main = do return ()
--  bracket (EventStore.connect EventStore.defaultSettings (EventStore.Static "127.0.0.1" 1113))
--          (\connection -> do EventStore.shutdown connection
--                             EventStore.waitTillClosed connection)
--          (\connection -> sandbox connection)
--
--
--sandbox :: EventStore.Connection -> IO()
--sandbox eventStoreConnection = do
--           let logger = "[gsd.persist.command.request]"
--           updateGlobalLogger logger $ setLevel INFO
--           infoM logger $ "[Sandbox] - starting streams"
--           runStream $ parallely $ (WorkspaceStream.streamAllInfinitely eventStoreConnection)
--             & S.mapM (\(offset,workspaceId) -> do
--                threadId <- myThreadId
--                liftIO $ infoM logger $ (show threadId) ++ " > bootstrap workspace ID  -" ++ (show workspaceId)
--                runStream $ serially $ yieldAndSubscribeToNewUpdates eventStoreConnection (offset,workspaceId)
--                  & S.mapM (\(offset,workspaceId) -> do
--                      threadId <- myThreadId
--                      liftIO $ infoM logger $ (show threadId) ++ " > process workspace commands  -" ++ (show workspaceId))
--                 )
--
--
--yieldAndSubscribeToNewUpdates :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO (Offset, WorkspaceId)), Semigroup (stream IO Command ))  => EventStore.Connection -> (Offset , WorkspaceId) -> stream IO (Offset , WorkspaceId)
--yieldAndSubscribeToNewUpdates eventStoreConnection (offset,workspaceId) = S.yield (offset,workspaceId) <>  subscribeToNewUpdates eventStoreConnection (offset,workspaceId)
--
--subscribeToNewUpdates :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO Command ))  => EventStore.Connection -> (Offset , WorkspaceId) -> stream IO (Offset , WorkspaceId)
--subscribeToNewUpdates eventStoreConnection (offset,workspaceId) = ((CommandStream.subscribeToNewCommand eventStoreConnection workspaceId) & S.map (\command -> (offset,workspaceId)))



