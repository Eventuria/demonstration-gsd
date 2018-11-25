{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.Aggregate.Commands.Responses.CommandResponse where

import Cqrs.Aggregate.Commands.CommandId
import Data.Aeson
import qualified Data.Text as Text
import Cqrs.Aggregate.Ids.AggregateId

type RejectionReason = String

data CommandResponse  = CommandSuccessfullyProcessed { commandId :: CommandId , aggregateId :: AggregateId }
                      | CommandSkippedBecauseAlreadyProcessed { commandId :: CommandId , aggregateId ::AggregateId }
                      | CommandFailed { commandId :: CommandId , aggregateId ::AggregateId , reason :: RejectionReason}  deriving (Show,Eq)

commandResponseNameForCommandSuccessfullyProcessed :: String
commandResponseNameForCommandSuccessfullyProcessed = "commandSuccessfullyProcessed"

commandResponseNameForCommandSkippedBecauseAlreadyProcessed :: String
commandResponseNameForCommandSkippedBecauseAlreadyProcessed = "commandSkippedBecauseAlreadyProcessed"

commandResponseNameForCommandFailed :: String
commandResponseNameForCommandFailed = "commandFailed"

class CommandResponseSerializable response where
  getCommandResponseName :: response -> String
  getCommandId :: response -> CommandId
  getAggregateId :: response -> AggregateId


instance CommandResponseSerializable CommandResponse where
  getCommandResponseName CommandSuccessfullyProcessed {} = commandResponseNameForCommandSuccessfullyProcessed
  getCommandResponseName CommandSkippedBecauseAlreadyProcessed {} = commandResponseNameForCommandSkippedBecauseAlreadyProcessed
  getCommandResponseName CommandFailed {} = commandResponseNameForCommandFailed

  getCommandId CommandSuccessfullyProcessed { commandId = commandId} = commandId
  getCommandId CommandSkippedBecauseAlreadyProcessed { commandId = commandId} = commandId
  getCommandId CommandFailed { commandId = commandId} = commandId

  getAggregateId CommandSuccessfullyProcessed { aggregateId = aggregateId} = aggregateId
  getAggregateId CommandSkippedBecauseAlreadyProcessed { aggregateId = aggregateId} = aggregateId
  getAggregateId CommandFailed { aggregateId = aggregateId} = aggregateId

instance ToJSON CommandResponse where
   toJSON (commandResponse @ (CommandSuccessfullyProcessed commandId aggregateId)) = object [
          "commandId" .= commandId,
          "aggregateId" .= aggregateId,
          "commandResponseName" .= getCommandResponseName commandResponse]
   toJSON (commandResponse @ (CommandSkippedBecauseAlreadyProcessed commandId aggregateId)) = object [
             "commandId" .= commandId,
             "aggregateId" .= aggregateId,
             "commandResponseName" .= getCommandResponseName commandResponse]
   toJSON (commandResponse @ (CommandFailed commandId aggregateId reason)) = object [
             "commandId" .= commandId,
             "aggregateId" .= aggregateId,
             "commandResponseName" .= getCommandResponseName commandResponse,
             "reason" .= reason]

instance FromJSON CommandResponse  where

  parseJSON (Object jsonObject) = do
               commandResponseNameMaybe <- jsonObject .: "commandResponseName"
               case commandResponseNameMaybe of
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandSuccessfullyProcessed -> CommandSuccessfullyProcessed
                        <$> jsonObject .: "commandId"
                        <*> jsonObject .: "aggregateId"
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandSkippedBecauseAlreadyProcessed -> CommandSkippedBecauseAlreadyProcessed
                                            <$> jsonObject .: "commandId"
                                            <*> jsonObject .: "aggregateId"
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandFailed -> CommandFailed
                        <$> jsonObject .: "commandId"
                        <*> jsonObject .: "aggregateId"
                        <*> jsonObject .: "ideaContent"
                    Just (String unknownCommandResponseName) -> error $ "Command Response unknown : " ++ Text.unpack unknownCommandResponseName
                    Nothing -> error $ "Command Response name not provided"
                    _ -> error $ "Json format not expected"
  parseJSON _ = error $ "Json format not expected"