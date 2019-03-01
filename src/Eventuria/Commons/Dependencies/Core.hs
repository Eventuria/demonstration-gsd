module Eventuria.Commons.Dependencies.Core where

import Eventuria.Commons.DevOps.Core
import Data.List.NonEmpty

type Name = String
data UnhealthyDependency = UnhealthyDependency {name :: Name,
                                                unhealthyReason :: UnhealthyReason} deriving Show

type HealthCheck dependencies = dependencies -> IO (Either (NonEmpty UnhealthyDependency) Healthy)

type ExecutionUnderDependenciesAcquired dependencies c = (dependencies -> IO c)

type GetDependencies settings dependencies c = settings ->
                                                  ExecutionUnderDependenciesAcquired dependencies c ->
                                                  IO c