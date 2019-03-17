{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.CQRS.Write.Serialization.CommandTransaction where

import Data.Aeson
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Transaction.CommandTransaction
import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandling.Definition
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writable
import Eventuria.Libraries.CQRS.Write.Serialization.Event()
import Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()
import qualified Data.Text as Text

instance Writable CommandTransaction  where
  getItemName commandTransaction  = "commandTransaction"

instance ToJSON CommandTransaction where
   toJSON (CommandTransaction{commandId,
                              commandOffset,
                              aggregateId,
                              commandHandlingResult} ) = object [
      "commandId" .= commandId,
      "commandOffset" .= commandOffset,
      "aggregateId" .= aggregateId,
      "commandHandlingResult" .= commandHandlingResult]

instance FromJSON CommandTransaction where

    parseJSON (Object jsonObject) = CommandTransaction <$> jsonObject .: "commandId"
                                                       <*> jsonObject .: "commandOffset"
                                                       <*> jsonObject .: "aggregateId"
                                                       <*> jsonObject .: "commandHandlingResult"
    parseJSON _ =  error $ "Json format not expected"





resultTypeForCommandValidated :: String
resultTypeForCommandValidated = "commandValidated"

resultTypeForCommandRejected :: String
resultTypeForCommandRejected = "commandRejected"

instance Writable CommandHandlingResult where
  getItemName CommandValidated {} = resultTypeForCommandValidated
  getItemName CommandRejected {} = resultTypeForCommandRejected


instance ToJSON CommandHandlingResult where
   toJSON ((CommandValidated events )) = object [
          "resultType" .= resultTypeForCommandValidated,
          "events" .= events]
   toJSON ((CommandRejected reason)) = object [
         "resultType" .= resultTypeForCommandRejected,
         "reason" .= reason]

instance FromJSON CommandHandlingResult  where

  parseJSON (Object jsonObject) = do
               resultTypeMaybe <- jsonObject .: "resultType"
               case resultTypeMaybe of
                    Just (String resultType) | (Text.unpack resultType) == resultTypeForCommandValidated -> CommandValidated <$> jsonObject .: "events"
                    Just (String resultType) | (Text.unpack resultType) == resultTypeForCommandRejected  -> CommandRejected <$> jsonObject .: "reason"
                    Just (String unknown) -> error $ "Command Result Type unknown : " ++ Text.unpack unknown
                    Nothing -> error $ "Command Result Type name not provided"
                    _ -> error $ "Json format not expected"
  parseJSON _ = error $ "Json format not expected"