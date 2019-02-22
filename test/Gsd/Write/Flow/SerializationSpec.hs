{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Gsd.Write.Flow.SerializationSpec (main, spec)  where


import Test.Hspec
import Test.QuickCheck
import Gsd.Write.Model.Commands.Serialization ()
import Gsd.Write.Model.Commands.Command
import Data.Aeson
import Gsd.Write.Flow.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd Command" $ do
    it "can be marshalled and unmarshalled"
      $  verbose
      $ \command -> ((decode . encode) command) == (Just (command) :: Maybe (GsdCommand))

