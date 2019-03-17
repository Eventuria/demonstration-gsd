{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Eventuria.GSD.Write.Model.WriteModel where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.List as List

import Data.Aeson
import GHC.Generics
import Eventuria.GSD.Write.Model.Core

type RefinedGoalDescription = Text.Text
type ActionDetails = Text.Text
type GoalDescription = Text.Text

data GoalStatus = Created | InProgress | Paused | Accomplished | GivenUp deriving (Show , Eq , Generic )


data ActionStatus = Initiated | Completed deriving (Show , Eq , Generic )

data Action = Action {actionId :: ActionId, index :: Int , details :: ActionDetails ,status :: ActionStatus } deriving (Show , Eq , Generic )

instance Ord Action where
  (<=) a b =  (index a) <= (index b)

data Goal = Goal {workspaceId ::WorkspaceId ,
                  goalId :: GoalId ,
                  description :: GoalDescription ,
                  actions :: Set.Set Action,
                  status :: GoalStatus} deriving (Show , Eq , Generic )


data GsdWriteModel = GsdWriteModel { goals :: [Goal]} deriving (Show,Eq,Generic)


instance ToJSON GsdWriteModel where
  toJSON (GsdWriteModel {goals } ) = object [
            "goals" .= goals]

instance FromJSON GsdWriteModel  where

    parseJSON (Object jsonObject) = GsdWriteModel <$> jsonObject .: "goals"
    parseJSON _ =  error $ "Json format not expected"


instance ToJSON Goal where
  toJSON (Goal {workspaceId, goalId ,description,actions, status } ) = object [
            "workspaceId" .= workspaceId,
            "goalId" .= goalId,
            "description" .= description,
            "actions" .= actions,
            "status" .= status]

instance FromJSON Action  where

    parseJSON (Object jsonObject) = Action <$> jsonObject .: "actionId" <*>  jsonObject .: "index" <*>  jsonObject .: "details" <*>  jsonObject .: "status"
    parseJSON _ =  error $ "Json format not expected"

instance ToJSON Action where
  toJSON (Action {actionId, index, details, status } ) = object [
            "actionId" .= actionId,
            "index" .= index,
            "details" .= details,
            "status" .= status]

instance ToJSON ActionStatus where
    toJSON (Initiated) = String "initiated"
    toJSON (Completed) = String "completed"

instance FromJSON ActionStatus  where

  parseJSON (String s) = case Text.unpack s of
          "initiated" -> return Initiated
          "completed" -> return Completed
          _ -> error $ "FromJSON ActionStatus : Json format not expected"
  parseJSON _ =  error $ "FromJSON ActionStatus : Json format not expected"

instance FromJSON Goal  where

    parseJSON (Object jsonObject) = Goal <$> jsonObject .: "workspaceId" <*>  jsonObject .: "goalId" <*>  jsonObject .: "description" <*>  jsonObject .: "actions" <*>  jsonObject .: "status"
    parseJSON _ =  error $ "Json format not expected"


instance ToJSON GoalStatus where
    toJSON (Created) = String "created"
    toJSON (InProgress) = String "inProgress"
    toJSON (Paused) = String "paused"
    toJSON (Accomplished) = String "accomplished"
    toJSON (GivenUp) = String "givenUp"


instance FromJSON GoalStatus  where

 parseJSON (String s) = case Text.unpack s of
    "created" -> return Created
    "inProgress" -> return InProgress
    "paused" -> return Paused
    "accomplished" -> return Accomplished
    "givenUp" -> return GivenUp
    _ -> error $ "FromJSON GoalStatus : Json format not expected"
 parseJSON _ =  error $ "FromJSON GoalStatus : Json format not expected"


findGoal :: GoalId -> [Goal] -> Maybe Goal
findGoal  goalIdToFind goals = List.find (\Goal{goalId} -> goalIdToFind == goalId ) goals

findAction :: ActionId -> Set.Set Action -> Maybe Action
findAction  actionIdToFind actions = List.find (\Action{actionId} -> actionIdToFind == actionId ) actions

updateGoalStatus :: GoalId -> GoalStatus -> [Goal] -> [Goal]
updateGoalStatus goalIdToUpdate newGoalStatus goals =
  map (\goal@Goal{workspaceId,goalId,description,actions} -> case (goalIdToUpdate == goalId) of
    True -> Goal{workspaceId,goalId, description, actions, status = newGoalStatus}
    False -> goal
  ) $ goals

updateGoalWithNewRefinedGoalDescription :: GoalId -> RefinedGoalDescription -> [Goal] -> [Goal]
updateGoalWithNewRefinedGoalDescription goalIdToUpdate refinedGoalDescription goals =
  map (\goal@Goal{workspaceId,goalId,actions,status} -> case (goalIdToUpdate == goalId) of
    True -> Goal{workspaceId,goalId, actions,description = refinedGoalDescription,status}
    False -> goal
  ) $ goals

addAction :: [Goal] -> GoalId -> ActionId -> ActionDetails -> [Goal]
addAction goals goalId actionId actionDetails = case (findGoal goalId goals) of
   Nothing -> error ("logique issue in the write model")
   Just goal @ Goal {actions}  -> updateAction goalId
                                               Action {actionId,
                                                       index = Set.size actions,
                                                       details = actionDetails,
                                                       status = Initiated}
                                               goals
  where
    updateAction :: GoalId -> Action -> [Goal] -> [Goal]
    updateAction goalIdToUpdate action goals =
      map (\goal@Goal{workspaceId,goalId,description, status ,actions} -> case (goalIdToUpdate == goalId) of
        True -> Goal{workspaceId,goalId, description, status , actions = Set.union actions $ Set.fromList [action]}
        False -> goal
      ) $ goals

updateGoal :: [Goal] -> Goal -> [Goal]
updateGoal goals updatedGoal@Goal{goalId = goalIdToUpdate} =
 map (\goal@Goal{goalId} -> case (goalId == goalIdToUpdate) of
   True -> updatedGoal
   False -> goal
 ) $ goals

updateAction :: [Goal] -> GoalId -> ActionId -> ActionStatus -> [Goal]
updateAction goals goalId actionId actionStatus = case (findGoal goalId goals) of
   Nothing -> error ("logique issue in the write model")
   Just goal @ Goal {actions}  -> case (findAction actionId actions) of
      Nothing -> error ("logique issue in the write model")
      Just action @ Action {actionId,index,details,status}  ->
                  updateGoal
                  goals
                  (updateGoalWithUpdatedAction goal Action {status = actionStatus, ..})


 where

  updateGoalWithUpdatedAction :: Goal -> Action -> Goal
  updateGoalWithUpdatedAction Goal{workspaceId,goalId,description,status,actions} updatedAction @ Action {actionId = actionIdToUpdate} =
   Goal {workspaceId,goalId,description,status,
         actions = Set.map (\action@Action{actionId} -> case (actionId == actionIdToUpdate) of
                             True -> updatedAction
                             False -> action
                           ) $ actions}