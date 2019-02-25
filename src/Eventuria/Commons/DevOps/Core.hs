module Eventuria.Commons.DevOps.Core where

type UnhealthyReason = String
type Healthy = ()
type HealthCheckResult = Either UnhealthyReason Healthy

healthy :: HealthCheckResult
healthy = Right ()

unhealthy :: String -> HealthCheckResult
unhealthy = Left

