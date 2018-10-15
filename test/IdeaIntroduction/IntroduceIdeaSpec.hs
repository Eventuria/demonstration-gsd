{-# LANGUAGE QuasiQuotes #-}
module IdeaIntroduction.IntroduceIdeaSpec (main, spec)  where

import Data.Aeson.QQ
import Data.Aeson (Value)
import Test.Hspec
import Test.QuickCheck
import IdeaIntroduction.Commands
import Data.Aeson
import IdeaIntroduction.Generators

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Introduce Idea command" $ do
    it "can be marshalled and unmarshalled"
      $  verbose
      $ \introduceIdea -> (decode $ encode introduceIdea) == (Just (introduceIdea) :: Maybe IntroduceIdea)