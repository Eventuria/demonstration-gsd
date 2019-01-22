{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.CLI.BreadCrumbs where

import System.Console.Byline
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Data.Text

breadCrumbAtWorkspace :: Workspace ->  Byline IO ()
breadCrumbAtWorkspace workspace @ Workspace {
                                    workspaceId,
                                    workspaceName,
                                    goalStats = GoalStats {total,toBeAccomplished,accomplished},
                                    actionStats = ActionStats {total = totalActions,
                                                               completed,
                                                               opened}} =
   sayLn $
        fg green <> "Workspaces"
     <> fg white <>" / "
     <> fg cyan <> "\""
     <> fg cyan <> text workspaceName
     <> fg cyan <> "\"\n"
     <> fg white <>"> Todo  \n"
     <> fg green  <>"  - " <> (text . pack  .show) toBeAccomplished <> " goal(s)\n"
     <> fg green  <>"  - " <> (text . pack  .show) opened <> " action(s)\n"
     <> fg white <>"> Done  \n"
     <> fg green  <>"  - " <> (text . pack  .show) accomplished <> " goal(s)\n"
     <> fg green  <>"  - " <> (text . pack  .show) completed <> " action(s)\n"
     <> fg white <> "------------------------------------------"

breadCrumbAtGoal :: Workspace -> Goal ->  Byline IO ()
breadCrumbAtGoal workspace @ Workspace {workspaceId,workspaceName}
           goal      @ Goal {goalId,description} =
   sayLn $ fg white <> "You are here: "
     <> fg green <> "Workspaces"
     <> fg white <>" / "
     <> fg cyan <> "\""
     <> fg cyan <> text workspaceName
     <> fg cyan <> "\""
     <> fg white <>" / "
     <> fg green <> "Goals"
     <> fg white <>" / "
     <> fg cyan <> "\""
     <> fg cyan <> text description
     <> fg cyan <> "\""