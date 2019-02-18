module DevOps.Core where

type UnhealthyReason = String

type HealthCheckResult = Either UnhealthyReason ()

healthy :: HealthCheckResult
healthy = Right ()

unhealthy :: String -> HealthCheckResult
unhealthy = Left

