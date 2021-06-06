module IntervalTreeSpec where

import Control.Exception (evaluate)
import qualified Data.Bifunctor as Bifunc
import qualified Data.List as List
import Data.Maybe (isNothing, listToMaybe)
import qualified IntervalTree as IT
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (IT.Interval a) where
  arbitrary = fmap interval arbitrary
    where
      interval (a, b) = IT.Interval (min a b) (max a b)

instance (Ord a, Arbitrary a) => Arbitrary (IT.IntervalTree a) where
  arbitrary = fmap IT.fromList arbitrary

spec :: Spec
spec = do
  describe "IntervalTree.null/IntervalTree.empty" $ do
    it "IntervalTree.null IntervalTree.empty == True" $ do
      IT.null IT.empty `shouldBe` True

  describe "IntervalTree.null/IntervalTree.singleton" $ do
    prop "IntervalTree.null . IntervalTree.singleton == const False" $ do
      \x -> not $ IT.null (IT.singleton x :: IT.IntervalTree Int)

  describe "IntervalTree.size" $ do
    prop "IntervalTree.size == length . IntervalTree.toList" $ do
      \xs -> IT.size (xs :: IT.IntervalTree Int) == length (IT.toList xs)

  describe "IntervalTree.toList" $ do
    prop "IntervalTree.toList . IntervalTree.fromList == sort" $
      \xs ->
        (List.sort xs :: [IT.Interval Int]) == (IT.toList . IT.fromList $ xs)
    prop "IntervalTree.fromList . IntervalTree.toList == id" $
      \xs -> (xs :: IT.IntervalTree Int) == (IT.fromList . IT.toList $ xs)

  describe "IntervalTree.fromList / IntervalTree.fromFoldable" $ do
    prop "IntervalTree.fromList == IntervalTree.fromFoldable" $
      \xs -> IT.fromList (xs :: [IT.Interval Int]) == IT.fromFoldable xs

  describe "IntervalTree.fromAscList / IntervalTree.fromAscFoldable" $ do
    prop "IntervalTree.fromAscList == IntervalTree.fromAscFoldable" $
      \xs -> IT.fromAscList (xs :: [IT.Interval Int]) == IT.fromAscFoldable xs

  describe "IntervalTree.fromDescList / IntervalTree.fromDescFoldable" $ do
    prop "IntervalTree.fromDescList == IntervalTree.fromDescFoldable" $
      \xs -> IT.fromDescList (xs :: [IT.Interval Int]) == IT.fromDescFoldable xs

  describe "IntervalTree.fromAscList" $ do
    prop "IntervalTree.fromAscList . sort == IntervalTree.fromList" $
      \xs ->
        IT.fromAscList (List.sort xs :: [IT.Interval Int]) == IT.fromList xs

  describe "IntervalTree.fromDescList" $ do
    prop "IntervalTree.fromDescList . reverse . sort == IntervalTree.fromList" $
      \xs ->
        IT.fromDescList (reverse (List.sort xs :: [IT.Interval Int]))
          == IT.fromList xs

  describe "IntervalTree.insert" $ do
    prop
      "IntervalTree.toList . IntervalTree.insert x == sort . (x:) . IntervalTree.toList"
      $ \x xs ->
        IT.toList (IT.insert x xs :: IT.IntervalTree Int)
          == (List.sort . (x :) . IT.toList $ xs)

  describe "IntervalTree.delete" $ do
    prop
      "IntervalTree.delete x . IntervalTree.fromList == IntervalTree.fromList . delete x"
      $ \x xs ->
        IT.delete x (IT.fromList xs :: IT.IntervalTree Int)
          == IT.fromList (List.delete x xs)

  describe "IntervalTree.delete" $ do
    prop
      "IntervalTree.member x == elem x . IntervalTree.toList"
      $ \x xs ->
        IT.member x (xs :: IT.IntervalTree Int)
          == (x `elem` IT.toList xs)

  describe "IntervalTree.overlappingInterval" $ do
    prop
      "IntervalTree.overlappingInterval x == find (\\y -> low x <= high y && low y <= high x) . IntervalTree.toList"
      $ \x xs ->
        IT.overlappingInterval x (xs :: IT.IntervalTree Int)
          == ( List.find (\y -> IT.low x <= IT.high y && IT.low y <= IT.high x)
                 . IT.toList
                 $ xs
             )

  describe "IntervalTree.overlappingIntervals" $ do
    prop
      "IntervalTree.overlappingIntervals x == filter (\\y -> low x <= high y && low y <= high x) . IntervalTree.toList"
      $ \x xs ->
        IT.overlappingIntervals x (xs :: IT.IntervalTree Int)
          == ( filter (\y -> IT.low x <= IT.high y && IT.low y <= IT.high x)
                 . IT.toList
                 $ xs
             )

  describe "IntervalTree.map/IntervalTree.mapMonotonic" $ do
    prop
      "IntervalTree.map f == IntervalTree.mapMonotonic f for monotonic functions"
      $ \xs ->
        (IT.map (* 2) xs :: IT.IntervalTree Int)
          == IT.mapMonotonic (* 2) xs

  describe "IntervalTree.map" $ do
    prop
      "IntervalTree.toList . IntervalTree.map f == mapInterval f . IntervalTree.toList"
      $ \xs ->
        IT.toList (IT.map (* 2) xs :: IT.IntervalTree Int)
          == map
            (\(IT.Interval x y) -> IT.Interval (x * 2) (y * 2))
            (IT.toList xs)

  describe "IntervalTree.><" $ do
    prop
      "IntervalTree.toList (xs IntervalTree.>< ys) == sort $ IntervalTree.toList xs ++ IntervalTree.toList ys"
      $ \xs ys ->
        (IT.toList (xs IT.>< ys) :: [IT.Interval Int])
          == List.sort (IT.toList xs ++ IT.toList ys)

  describe "IntervalTree.head" $ do
    it "throws an exception on the empty tree" $
      evaluate (IT.head IT.empty :: IT.Interval Int)
        `shouldThrow` errorCall "IntervalTree.head: empty IntervalTree"
    prop "IntervalTree.head . IntervalTree.fromList == minimum" $
      \xs ->
        not (null xs)
          ==> IT.head (IT.fromList xs :: IT.IntervalTree Int) == minimum xs

  describe "IntervalTree.tail" $ do
    it "throws an exception on the empty tree" $
      evaluate (IT.tail IT.empty :: IT.IntervalTree Int)
        `shouldThrow` errorCall "IntervalTree.tail: empty IntervalTree"
    prop
      "IntervalTree.tail . IntervalTree.fromList == IntervalTree.fromList . tail . sort"
      $ \xs ->
        not (null xs)
          ==> IT.tail (IT.fromList xs :: IT.IntervalTree Int)
          == (IT.fromList . tail . List.sort $ xs)

  describe "IntervalTree.last" $ do
    it "throws an exception on the empty tree" $
      evaluate (IT.last IT.empty :: IT.Interval Int)
        `shouldThrow` errorCall "IntervalTree.last: empty IntervalTree"
    prop "IntervalTree.last . IntervalTree.fromList == maximum" $
      \xs ->
        not (null xs)
          ==> IT.last (IT.fromList xs :: IT.IntervalTree Int) == maximum xs

  describe "IntervalTree.init" $ do
    it "throws an exception on the empty tree" $
      evaluate (IT.init IT.empty :: IT.IntervalTree Int)
        `shouldThrow` errorCall "IntervalTree.init: empty IntervalTree"
    prop
      "IntervalTree.init . IntervalTree.fromList == IntervalTree.fromList . init . sort"
      $ \xs ->
        not (null xs)
          ==> IT.init (IT.fromList xs :: IT.IntervalTree Int)
          == (IT.fromList . init . List.sort $ xs)

  describe "IntervalTree.lookup" $ do
    prop "invalid: IntervalTree.lookup i == Nothing" $
      \xs -> forAll (choose (IT.size xs, IT.size xs + 2)) $
        \i -> isNothing (IT.lookup i xs :: Maybe (IT.Interval Int))
    prop
      "valid: IntervalTree.lookup i . IntervalTree.fromList == Just . (!! i) . sort"
      $ \xs ->
        not (null xs)
          ==> forAll (choose (0, length xs - 1))
          $ \i ->
            (IT.lookup i (IT.fromList xs) :: Maybe (IT.Interval Int))
              == Just (List.sort xs !! i)

  describe "IntervalTree.!?" $ do
    prop "IntervalTree.!!? == flip IntervalTree.lookup" $
      \xs -> forAll (choose (-1, IT.size xs + 1)) $
        \i -> (xs IT.!? i :: Maybe (IT.Interval Int)) == IT.lookup i xs

  describe "IntervalTree.index" $ do
    prop "IntervalTree.index i . IntervalTree.fromList == (!! i) . sort" $
      \xs ->
        not (null xs)
          ==> forAll (choose (0, length xs - 1))
          $ \i ->
            (IT.index (IT.fromList xs) i :: IT.Interval Int)
              == List.sort xs !! i

  describe "IntervalTree.drop" $ do
    prop "IntervalTree.drop i == snd . IntervalTree.splitAt i" $
      \xs -> forAll (choose (-1, IT.size xs)) $
        \i -> (IT.drop i xs :: IT.IntervalTree Int) == (snd . IT.splitAt i $ xs)

  describe "IntervalTree.take" $ do
    prop "IntervalTree.take i == fst . IntervalTree.splitAt i" $
      \xs -> forAll (choose (-1, IT.size xs)) $
        \i -> (IT.take i xs :: IT.IntervalTree Int) == (fst . IT.splitAt i $ xs)

  describe "IntervalTree.splitAt" $ do
    prop
      "bimap IntervalTree.toList IntervalTree.toList . IntervalTree.splitAt i == splitAt i . IntervalTree.toList"
      $ \xs -> forAll (choose (-1, IT.size xs)) $
        \i ->
          Bifunc.bimap
            IT.toList
            IT.toList
            (IT.splitAt i (xs :: IT.IntervalTree Int))
            == splitAt i (IT.toList xs)

  describe "IntervalTree.partition" $ do
    prop
      "bimap IntervalTree.toList IntervalTree.toList . IntervalTree.partition p == partition p . IntervalTree.toList"
      $ \xs ->
        Bifunc.bimap
          IT.toList
          IT.toList
          ( IT.partition
              (\x -> even $ IT.high x - IT.low x)
              (xs :: IT.IntervalTree Int)
          )
          == List.partition (\x -> even $ IT.high x - IT.low x) (IT.toList xs)

  describe "IntervalTree.filter" $ do
    prop
      "IntervalTree.toList . IntervalTree.filter p == filter p . IntervalTree.toList"
      $ \xs ->
        IT.toList
          ( IT.filter
              (\x -> even $ IT.high x - IT.low x)
              (xs :: IT.IntervalTree Int)
          )
          == List.filter (\x -> even $ IT.high x - IT.low x) (IT.toList xs)
