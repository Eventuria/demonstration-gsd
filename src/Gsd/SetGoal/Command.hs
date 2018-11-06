{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.SetGoal.Command where

import GHC.Generics
import Cqrs.Core
import Cqrs.Command
import Gsd.Core
import Data.Aeson
import Data.Text

data SetGoal = SetGoal  {
                    commandId :: CommandId ,
                    workspaceId ::WorkspaceId ,
                    goalId :: GoalId ,
                    goalDetails :: String}

getSetGoalName = "setGoal" :: String

--instance Command SetGoal where
--  getCommandId = commandId
--  getAggregateId = workspaceId
--  getCommandName = getSetGoalName
--
--
--instance ToJSON SetGoal where
--
--   toJSON (SetGoal commandId workspaceId goalId goalDetails) = object [
--             "commandName" .= getSetGoalName,
--             "commandId" .= commandId,
--             "workspaceId" .= workspaceId,
--             "goalId" .= goalId,
--             "goalDetails" .= goalDetails]
--
--
--instance FromJSON SetGoal  where
--
--    parseJSON (Object jsonObject) = do
--             commandNameMaybe <- jsonObject .: "commandName"
--             case commandNameMaybe of
--                  Just (String commandName) | (unpack commandName) == getSetGoalName -> SetGoal
--                                        <$> jsonObject .: "commandId"
--                                        <*> jsonObject .: "workspaceId"
--                                        <*> jsonObject .: "goalId"
--                                        <*> jsonObject .: "goalDetails"
--                  Just (String unknownCommandName) -> error $ "Serializing something else than SetGoal command : " ++ unpack unknownCommandName
--                  Nothing -> error $ "Command name not provided"