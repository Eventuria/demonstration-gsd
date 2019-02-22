module Eventuria.Commons.Dependencies.Core where

import Eventuria.Commons.DevOps.Core
import Data.List.NonEmpty

type Name = String
data UnhealthyDependency = UnhealthyDependency {name :: Name,
                                                unhealthyReason :: UnhealthyReason}

type ExecutionUnderDependenciesAcquired dependencies c = (dependencies -> IO c)
type ExecutionIfDependenciesAcquisitionFailed c = (NonEmpty UnhealthyDependency -> IO c)

type RetrieveDependencies settings dependencies c = settings ->
                                                  ExecutionUnderDependenciesAcquired dependencies c ->
                                                  ExecutionIfDependenciesAcquisitionFailed c ->
                                                  IO c