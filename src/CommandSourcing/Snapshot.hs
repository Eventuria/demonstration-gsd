module CommandSourcing.Snapshot where
import CommandSourcing.Core
import Data.Set (Set)
import qualified Data.Set as Set


data Snapshot = Snapshot { lastOffsetConsumed::Offset , commandsProcessed :: Set CommandId, state :: WorkspaceState } deriving (Eq)

instance Show Snapshot where
  show snapshot = "Snapshot { offset = " ++ ( show $ lastOffsetConsumed snapshot)  ++ " }"

data WorkspaceState = WorkspaceState {workspaceId :: WorkspaceId } deriving (Eq,Show)
