{-# LANGUAGE OverloadedStrings #-}
module DevOps.Core where


import Data.Aeson
import Data.Text
type UnhealthyReason = String

data HealthCheckResult = Healthy
                       | Unhealthy UnhealthyReason



class MicroService service where
  healthCheck :: service -> IO HealthCheckResult


instance FromJSON HealthCheckResult  where

    parseJSON (Object jsonObject) = do
      status <- jsonObject .: "status"
      case status of
          Just (String status) | (unpack status) == "healthy" -> return Healthy
          Just (String status) | (unpack status) == "unhealthy" -> Unhealthy <$> jsonObject .: "reason"
          _ -> error $ "health check status undefined"
    parseJSON _ =  error $ "Json format not expected"


instance ToJSON HealthCheckResult where
  toJSON (Healthy) = String "healthy"
  toJSON (Unhealthy reason) = object[
                                "status" .= String "unhealthy",
                                "reason" .= reason]
