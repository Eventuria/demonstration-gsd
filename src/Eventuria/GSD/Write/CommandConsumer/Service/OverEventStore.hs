{-# LANGUAGE FlexibleContexts #-}
module Eventuria.GSD.Write.CommandConsumer.Service.OverEventStore  where

import           Control.Exception

import           Eventuria.Commons.Logger.Core

import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Write.CqrsInstance

import qualified Eventuria.GSD.Write.CommandConsumer.Service.Generic as GenericGsd
import           Eventuria.GSD.Write.Repository.EventStoreStreams

consumeCommands :: Logger -> EventStoreClient.Dependencies -> IO (Either SomeException())
consumeCommands logger eventStoreClientDependencies =
   GenericGsd.consumeCommands
      logger
      (getEventStoreStreamRepository eventStoreClientDependencies)
      getEventStoreReading
      getEventStoreWriting

