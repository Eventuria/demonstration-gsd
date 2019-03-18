module Eventuria.Commons.Dependencies.Core where

import Data.List.NonEmpty

type UnhealthyReason = String
type Healthy = ()
type HealthCheckResult = Either UnhealthyReason Healthy

healthy :: HealthCheckResult
healthy = Right ()

unhealthy :: String -> HealthCheckResult
unhealthy = Left



type Name = String
data UnhealthyDependency = UnhealthyDependency {name :: Name,
                                                unhealthyReason :: UnhealthyReason} deriving Show

type HealthCheck dependencies = dependencies -> IO (Either (NonEmpty UnhealthyDependency) Healthy)

type ExecutionUnderDependenciesAcquired dependencies c = (dependencies -> IO c)

type GetDependencies settings dependencies c = settings ->
                                                  ExecutionUnderDependenciesAcquired dependencies c ->
                                                  IO c