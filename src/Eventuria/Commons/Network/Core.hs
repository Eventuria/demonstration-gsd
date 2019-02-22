module Eventuria.Commons.Network.Core where

type URLHost = String
type URLPort = Int
type URLPath = String

data URL = URL {host :: URLHost,port :: URLPort,path :: URLPath}