{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.QuestionGoal.Command where

import GHC.Generics
import Cqrs.Core
import Cqrs.Command
import Gsd.Core
import Data.Aeson
import Data.Text

data QuestionGoal = QuestionGoal {
                    commandId :: CommandId ,
                    workspaceId ::WorkspaceId ,
                    goalId :: GoalId ,
                    questionId :: QuestionId ,
                    questionDetails :: String}

getQuestionGoalName = "questionGoal" :: String

--instance Command QuestionGoal where
--  getCommandId = commandId
--  getAggregateId = workspaceId
--  getCommandName = getQuestionGoalName
--
--
--instance ToJSON QuestionGoal where
--
--   toJSON (QuestionGoal commandId workspaceId goalId questionId questionDetails ) = object [
--                "commandName" .= getQuestionGoalName,
--                "commandId" .= commandId,
--                "workspaceId" .= workspaceId,
--                "goalId" .= goalId,
--                "questionId" .= questionId,
--                "questionDetails" .= questionDetails]
--
--
--instance FromJSON QuestionGoal  where
--
--    parseJSON (Object jsonObject) = do
--             commandNameMaybe <- jsonObject .: "commandName"
--             case commandNameMaybe of
--                  Just (String commandName) | (unpack commandName) == getQuestionGoalName -> QuestionGoal
--                                        <$> jsonObject .: "commandId"
--                                        <*> jsonObject .: "workspaceId"
--                                        <*> jsonObject .: "goalId"
--                                        <*> jsonObject .: "questionId"
--                                        <*> jsonObject .: "questionDetails"
--                  Just (String unknownCommandName) -> error $ "Serializing something else than QuestionGoal command : " ++ unpack unknownCommandName
--                  Nothing -> error $ "Command name not provided"


--data ActionizeOnQuestion = ActionizeOnQuestion {
--                    commandId :: CommandId ,
--                    workspaceId ::WorkspaceId ,
--                    questionId :: UUID ,
--                    actionId :: UUID ,
--                    actionDetails :: String}
--
--data AnswerToQuestion = AnswerToQuestion {
--                    commandId :: CommandId ,
--                    workspaceId ::WorkspaceId ,
--                    questionId :: UUID ,
--                    actionId :: UUID ,
--                    answerDetails :: String}
--
--data ActionizeOnTheGoalDirectly = ActionizeOnTheGoalDirectly {
--                    commandId :: CommandId ,
--                    workspaceId ::WorkspaceId ,
--                    goalId :: UUID ,
--                    actionId :: UUID ,
--                    actionDetails :: String}  deriving (Show,Eq, Generic)

--getActionizeOnQuestionName = "actionizeOnQuestion" :: String
--getAnswerToQuestionName = "answerToQuestion" :: String
--getActionizeOnTheGoalDirectlyName = "actionizeOnTheGoalDirectly" :: String
--
--
--instance ToJSON Command where
--
--   toJSON (command @ (ActionizeOnQuestion commandId workspaceId questionId actionId actionDetails )) = object [
--             "commandName" .= getCommandName command,
--             "commandId" .= commandId,
--             "workspaceId" .= workspaceId,
--             "questionId" .= questionId,
--             "actionId" .= actionId,
--             "actionDetails" .= actionDetails]
--   toJSON (command @ (AnswerToQuestion commandId workspaceId questionId actionId answerDetails )) = object [
--             "commandName" .= getCommandName command,
--             "commandId" .= commandId,
--             "workspaceId" .= workspaceId,
--             "questionId" .= questionId,
--             "actionId" .= actionId,
--             "answerDetails" .= answerDetails]
--   toJSON (command @ (ActionizeOnTheGoalDirectly commandId workspaceId goalId actionId actionDetails)) = object [
--             "commandName" .= getCommandName command,
--             "commandId" .= commandId,
--             "workspaceId" .= workspaceId,
--             "goalId" .= goalId,
--             "actionId" .= actionId,
--             "actionDetails" .= actionDetails]
--
--
--instance FromJSON Command  where
--
--    parseJSON (Object jsonObject) = do
--             commandNameMaybe <- jsonObject .: "commandName"
--             case commandNameMaybe of
--
--                  Just (String commandName) | (unpack commandName) == getActionizeOnQuestionName -> ActionizeOnQuestion
--                      <$> jsonObject .: "commandId"
--                      <*> jsonObject .: "workspaceId"
--                      <*> jsonObject .: "questionId"
--                      <*> jsonObject .: "actionId"
--                      <*> jsonObject .: "actionDetails"
--                  Just (String commandName) | (unpack commandName) == getAnswerToQuestionName -> AnswerToQuestion
--                      <$> jsonObject .: "commandId"
--                      <*> jsonObject .: "workspaceId"
--                      <*> jsonObject .: "questionId"
--                      <*> jsonObject .: "actionId"
--                      <*> jsonObject .: "answerDetails"
--                  Just (String commandName) | (unpack commandName) == getActionizeOnTheGoalDirectlyName -> ActionizeOnTheGoalDirectly
--                      <$> jsonObject .: "commandId"
--                      <*> jsonObject .: "workspaceId"
--                      <*> jsonObject .: "goalId"
--                      <*> jsonObject .: "actionId"
--                      <*> jsonObject .: "actionDetails"
--                  Just (String unknownCommandName) -> error $ "Command unknown : " ++ unpack unknownCommandName
--                  Nothing -> error $ "Command name not provided"
--
--
--serializedCommandNameForCreateWorkspace = "createWorkspace"
--serializedCommandNameForIntroduceIdea = "introduceIdea"