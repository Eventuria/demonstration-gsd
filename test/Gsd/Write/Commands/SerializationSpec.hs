{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Gsd.Write.Commands.SerializationSpec (main, spec)  where


import Test.Hspec
import Test.QuickCheck
import Gsd.Write.Commands.Serialization ()
import Gsd.Write.Commands.Command
import Data.Aeson
import Gsd.Write.Commands.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd Command" $ do
    it "can be marshalled and unmarshalled"
      $  verbose
      $ \command -> ((decode . encode) command) == (Just (command) :: Maybe (GsdCommand))

