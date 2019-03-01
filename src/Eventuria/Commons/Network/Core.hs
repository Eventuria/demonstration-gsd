module Eventuria.Commons.Network.Core where

import Control.Exception hiding (Handler)
import Control.Concurrent
import Data.Either.Combinators
import Eventuria.Commons.Logger.Core

type URLHost = String
type URLPort = Int
type URLPath = String

data URL = URL {host :: URLHost,port :: URLPort,path :: URLPath}

type ServerThreadId = ThreadId
data ServerDownException = ServerDownException deriving Show

instance Exception ServerDownException


breakServerOnFailure :: Logger -> ServerThreadId -> Either SomeException result -> IO (result)
breakServerOnFailure logger serverThreadId result = (breakOnLeft result) >>= return . fromRight'
 where breakOnLeft :: Either SomeException result -> IO (Either SomeException result)
       breakOnLeft = either
                      (\someException -> do
                            logInfo logger $ "breaking server (thread : "++ show serverThreadId ++ " ) by raising an exception on server request"
                            throwTo serverThreadId $ ServerDownException
                            return $ Left someException)
                      (\value -> return $ Right value)