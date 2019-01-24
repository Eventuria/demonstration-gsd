{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.CLI.SectionHeaders where

import System.Console.Byline
import Gsd.Read.Workspace
import Gsd.Read.Goal


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