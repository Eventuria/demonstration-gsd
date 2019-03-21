{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Write.Model.WriteModel.WriteModelSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck

import Data.Aeson

import Eventuria.GSD.Write.Model.WriteModel.PropertyTesting.WriteModel ()
import Eventuria.GSD.Write.Model.WriteModel

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd write Model" $ do
    it "can be marshalled and unmarshalled"
      $ property
      $ \gsdState -> ((decode . encode) gsdState) == (Just (gsdState) :: Maybe (GsdWriteModel))
