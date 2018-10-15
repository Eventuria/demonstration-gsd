module IdeaIntroduction.Generators where

import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances.UUID
import Test.QuickCheck.Instances.Time
import IdeaIntroduction.Events
import IdeaIntroduction.Commands
import GHC.Generics


introduceIdeaGen :: Gen IntroduceIdea
introduceIdeaGen = IntroduceIdea  <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary IntroduceIdea where
  arbitrary = introduceIdeaGen


eventGen :: Gen Event
eventGen = IdeaIntroduced  <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Event where
  arbitrary = eventGen