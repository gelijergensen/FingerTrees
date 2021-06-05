module DequeSpec where

import Control.Exception (evaluate)
import qualified Deque as D
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary a => Arbitrary (D.Deque a) where
  arbitrary = fmap D.fromList arbitrary

spec :: Spec
spec = do
  -- describe "Prelude.head" $ do
  --   it "returns the first element of a list" $ do
  --     head [23 ..] `shouldBe` (23 :: Int)

  --   it "returns the first element of an *arbitrary* list" $
  --     property $ \x xs -> head (x : xs) == (x :: Int)

  --   it "throws an exception if used with an empty list" $ do
  --     evaluate (head []) `shouldThrow` anyException

  describe "Deque.empty" $ do
    it "Deque.empty == Deque.Empty" $ do
      D.empty `shouldBe` (D.Empty :: D.Deque Int)

  describe "Deque.singleton" $ do
    prop "Deque.singleton == (Deque.:<| Deque.empty)" $
      \x -> (D.singleton x :: D.Deque Int) == x D.:<| D.empty
    prop "Deque.singleton == (Deque.empty Deque.:|>)" $
      \x -> (D.singleton x :: D.Deque Int) == D.empty D.:|> x

  describe "Deque.toList" $ do
    prop "toList . fromList == id" $
      \xs -> (xs :: [Int]) == (D.toList . D.fromList $ xs)

    prop "fromList . toList == id" $
      \xs -> (xs :: D.Deque Int) == (D.fromList . D.toList $ xs)
