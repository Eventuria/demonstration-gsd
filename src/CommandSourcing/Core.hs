
module CommandSourcing.Core where
import Data.UUID
import Data.Time


type CommandId = UUID
type WorkspaceId = UUID
type IdeaId = UUID
type Offset = Integer

data Idea = Idea { workSpaceId :: WorkspaceId ,ideaId :: IdeaId , createdOn :: UTCTime , ideaContent :: String } deriving (Eq,Show)