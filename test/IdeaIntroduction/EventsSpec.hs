module IdeaIntroduction.EventsSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck
import IdeaIntroduction.Events
import Data.Aeson
import IdeaIntroduction.Generators

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Events" $ do
    it "can be marshalls and unmarshalls"
      $  property
      $ \events -> (decode $ encode events) == (Just (events) :: Maybe Event)