{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.CLI.BreadCrumbs where

import System.Console.Byline
import Gsd.Read.Workspace
import Gsd.Read.Goal

breadCrumb :: Workspace -> Goal -> Stylized
breadCrumb workspace @ Workspace {workspaceId,workspaceName}
           goal      @ Goal {goalId,description} =
   fg white <> "You are here: "
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