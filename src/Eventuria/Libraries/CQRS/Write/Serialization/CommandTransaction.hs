{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.CQRS.Write.Serialization.CommandTransaction where

import Data.Aeson
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Transaction.CommandTransaction
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writable
import Eventuria.Libraries.CQRS.Write.Serialization.Event()
import Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()
import qualified Data.Text as Text


instance ToJSON writeModel => Writable (CommandTransaction writeModel) where
  getItemName commandTransaction  = "commandTransaction"

instance ToJSON writeModel => ToJSON (CommandTransaction writeModel) where
   toJSON (CommandTransaction{commandId,
                              aggregateId,
                              snapshot,
                              result} ) = object [
      "commandId" .= commandId,
      "aggregateId" .= aggregateId,
      "snapshot" .= snapshot,
      "result" .= result]

instance FromJSON writeModel => FromJSON (CommandTransaction writeModel) where

    parseJSON (Object jsonObject) = CommandTransaction <$> jsonObject .: "commandId"
                                                       <*> jsonObject .: "aggregateId"
                                                       <*> jsonObject .: "snapshot"
                                                       <*> jsonObject .: "result"
    parseJSON _ =  error $ "Json format not expected"


instance ToJSON writeModel => ToJSON (Snapshot writeModel) where
   toJSON Snapshot{offset, writeModelMaybe} = object [
      "offset" .= offset,
      "writeModelMaybe" .= writeModelMaybe]

instance FromJSON writeModel => FromJSON (Snapshot writeModel) where

    parseJSON (Object jsonObject) = Snapshot <$> jsonObject .: "offset"
                                             <*> jsonObject .: "writeModelMaybe"

    parseJSON _ =  error $ "Json format not expected"




resultTypeForCommandAccepted :: String
resultTypeForCommandAccepted = "commandAccepted"

resultTypeForCommandRejected :: String
resultTypeForCommandRejected = "commandRejected"

instance Writable CommandProcessResult where
  getItemName CommandAccepted {} = resultTypeForCommandAccepted
  getItemName CommandRejected {} = resultTypeForCommandRejected


instance ToJSON CommandProcessResult where
   toJSON (commandResponse @ (CommandAccepted events )) = object [
          "resultType" .= resultTypeForCommandAccepted,
          "events" .= events]
   toJSON (commandResponse @ (CommandRejected reason)) = object [
         "resultType" .= resultTypeForCommandRejected,
         "reason" .= reason]

instance FromJSON CommandProcessResult  where

  parseJSON (Object jsonObject) = do
               commandResponseNameMaybe <- jsonObject .: "resultType"
               case commandResponseNameMaybe of
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == resultTypeForCommandAccepted -> CommandAccepted <$> jsonObject .: "events"
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == resultTypeForCommandRejected -> CommandRejected <$> jsonObject .: "reason"
                    Just (String unknownCommandResponseName) -> error $ "Command Status unknown : " ++ Text.unpack unknownCommandResponseName
                    Nothing -> error $ "Command Response name not provided"
                    _ -> error $ "Json format not expected"
  parseJSON _ = error $ "Json format not expected"