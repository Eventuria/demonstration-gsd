module CreateWorkspaceSpec (main, spec)  where

import Data.Aeson.QQ
import Data.Aeson (Value)
import Test.Hspec
import Test.QuickCheck
import Generic.Random
import Data.Aeson
import Cqrs.Commands.Command
import Cqrs.Commands.Serialization
import Test.QuickCheck.Instances.UUID
import Network.Wreq

instance Arbitrary Command where
  arbitrary :: Gen Command
  arbitrary = genericArbitraryU

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "User" $ do
    it "can create workspace" $ property $
      \createWorkspaceCommand -> do
        response <- post "127.0.0.1:3000" $ createWorkspaceCommand :: Command
