module Eventuria.GSD.Write.CommandConsumer.Handling.CommandPredicates where


import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset


isFirstCommand :: Offset -> Bool
isFirstCommand 0 = True
isFirstCommand _ = False

isNotFirstCommand :: Offset -> Bool
isNotFirstCommand = not . isFirstCommand
