module SetSpec where

import Control.Exception (evaluate)
import qualified Data.Bifunctor as Bifunc
import qualified Data.List as List
import Data.Maybe (isNothing, listToMaybe)
import qualified Set as S
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (S.Set a) where
  arbitrary = fmap S.fromList arbitrary

spec :: Spec
spec = do
  describe "Set.null/Set.empty" $ do
    it "Set.null Set.empty == True" $ do
      S.null S.empty `shouldBe` True

  describe "Set.null/Set.singleton" $ do
    prop "Set.null . Set.singleton == const False" $ do
      \x -> not $ S.null (S.singleton x :: S.Set Int)

  describe "Set.size" $ do
    prop "Set.size == length . Set.toList" $ do
      \xs -> S.size (xs :: S.Set Int) == length (S.toList xs)

  describe "Set.toList" $ do
    prop "Set.toList . Set.fromList == nub . sort" $
      \xs -> List.nub (List.sort xs :: [Int]) == (S.toList . S.fromList $ xs)
    prop "Set.fromList . Set.toList == id" $
      \xs -> (xs :: S.Set Int) == (S.fromList . S.toList $ xs)

  describe "Set.fromList/Set.fromFoldable" $ do
    prop "Set.fromList == Set.fromFoldable" $
      \xs -> S.fromList (xs :: [Int]) == S.fromFoldable xs

  describe "Set.fromAscList/Set.fromAscFoldable" $ do
    prop "Set.fromAscList . sort == Set.fromAscFoldable . sort" $
      \xs ->
        S.fromAscList (List.sort xs :: [Int])
          == S.fromAscFoldable (List.sort xs)

  describe "Set.fromDistinctAscList/Set.fromDistinctAscFoldable" $ do
    prop
      "Set.fromDistinctAscList . nub . sort == Set.fromDistinctAscFoldable . nub . sort"
      $ \xs ->
        (S.fromDistinctAscList . List.nub . List.sort $ (xs :: [Int]))
          == (S.fromDistinctAscFoldable . List.nub . List.sort $ xs)

  describe "Set.fromDescList/Set.fromDescFoldable" $ do
    prop
      "Set.fromDescList . reverse . sort == Set.fromDescFoldable . reverse . sort"
      $ \xs ->
        (S.fromDescList . reverse . List.sort $ (xs :: [Int]))
          == (S.fromDescFoldable . reverse . List.sort $ xs)

  describe "Set.fromDistinctDescList/Set.fromDistinctDescFoldable" $ do
    prop
      "Set.fromDistinctDescList . reverse . nub . sort == Set.fromDistinctDescFoldable . reverse . nub . sort"
      $ \xs ->
        ( S.fromDistinctDescList . reverse . List.nub . List.sort $
            (xs :: [Int])
        )
          == (S.fromDistinctDescFoldable . reverse . List.nub . List.sort $ xs)

  describe "Set.fromAscList" $ do
    prop "Set.fromAscList . sort == Set.fromList" $
      \xs ->
        (S.fromAscList . List.sort $ (xs :: [Int])) == S.fromList xs

  describe "Set.fromDistinctAscList" $ do
    prop "Set.fromDistinctAscList . sort == Set.fromList" $
      \xs ->
        (S.fromDistinctAscList . List.nub . List.sort $ (xs :: [Int])) == S.fromList xs
  describe "Set.fromDescList" $ do
    prop "Set.fromDescList . reverse . sort == Set.fromList" $
      \xs ->
        (S.fromDescList . reverse . List.sort $ (xs :: [Int]))
          == S.fromList xs

  describe "Set.fromDistinctDescList" $ do
    prop "Set.fromDistinctDescList . reverse . nub . sort == Set.fromList" $
      \xs ->
        (S.fromDistinctDescList . reverse . List.nub . List.sort $ (xs :: [Int]))
          == S.fromList xs