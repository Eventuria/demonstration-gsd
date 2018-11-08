module Gsd.CommandPredicates where

import Cqrs.Snapshot
import Cqrs.Command
import Cqrs.Streams
import Gsd.Commands

commandsAreAllAlreadyProcessed :: Offset -> Maybe AggregateSnapshot -> Bool
commandsAreAllAlreadyProcessed offset snapshotMaybe = Just offset == (lastOffsetConsumed <$> snapshotMaybe)

isFirstCommandProcessed :: Command -> Maybe AggregateSnapshot -> Bool
isFirstCommandProcessed command snapshotMaybe = isCreateWorkspaceCommand command && snapshotMaybe == Nothing
