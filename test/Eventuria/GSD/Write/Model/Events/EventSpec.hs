{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Write.Model.Events.EventSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck

import Data.Aeson

import Eventuria.GSD.Write.Model.Events.PropertyTesting.Event ()
import Eventuria.GSD.Write.Model.Events.Serialization ()
import Eventuria.GSD.Write.Model.Events.Event

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd Events" $ do
    it "can be marshalled and unmarshalled"
      $ property
      $ \event -> ((decode . encode) event) == (Just (event) :: Maybe (GsdEvent))
