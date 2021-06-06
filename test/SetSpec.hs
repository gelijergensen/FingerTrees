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

  describe "Set.insert" $ do
    prop "Set.toList . Set.insert x == sort . nub . (x :) . Set.toList" $
      \x xs ->
        S.toList (S.insert x xs :: S.Set Int)
          == (List.sort . List.nub . (x :) . S.toList $ xs)

  describe "Set.delete" $ do
    prop "Set.toList . Set.delete x == delete x . Set.toList" $
      \x xs ->
        S.toList (S.delete x xs :: S.Set Int)
          == List.delete x (S.toList xs)

  describe "Set.member" $ do
    prop "Set.member x == elem x . Set.toList" $
      \x xs -> S.member x (xs :: S.Set Int) == (x `elem` S.toList xs)

  describe "Set.map/Set.mapMonotonic" $ do
    prop "Set.map f == Set.mapMonotonic f for monotonic functions" $
      \xs -> (S.map (* 2) xs :: S.Set Int) == S.mapMonotonic (* 2) xs

  describe "Set.map" $ do
    prop "Set.toList . Set.map f == map f . Set.toList" $
      \xs -> S.toList (S.map (* 2) xs :: S.Set Int) == map (* 2) (S.toList xs)

  describe "Set.union" $ do
    prop
      "Set.toList (xs `Set.union` ys) == nub . sort $ Set.toList xs ++ Set.toList ys"
      $ \xs ys ->
        (S.toList (xs `S.union` ys) :: [Int])
          == (List.nub . List.sort) (S.toList xs ++ S.toList ys)

  describe "Set.intersection" $ do
    prop
      "Set.toList (xs `Set.intersection` ys) == nub . sort $ Set.toList xs `intersect` Set.toList ys"
      $ \xs ys ->
        (S.toList (xs `S.intersection` ys) :: [Int])
          == (List.nub . List.sort) (S.toList xs `List.intersect` S.toList ys)

  describe "Set.difference" $ do
    prop
      "Set.toList (xs `Set.difference` ys) == nub . sort $ Set.toList xs \\ Set.toList ys"
      $ \xs ys ->
        (S.toList (xs `S.difference` ys) :: [Int])
          == (List.nub . List.sort) (S.toList xs List.\\ S.toList ys)

  describe "Set.areDisjoint" $ do
    prop "Set.areDisjoint xs == Set.null . Set.intersection xs" $
      \xs ys ->
        S.areDisjoint (xs :: S.Set Int) (ys :: S.Set Int)
          == S.null (S.intersection xs ys)

  describe "Set.isSubsetOf" $
    prop "Set.isSubsetOf xs == Set.null . Set.difference xs" $
      \xs ys ->
        S.isSubsetOf (xs :: S.Set Int) (ys :: S.Set Int)
          == S.null (S.difference xs ys)

  describe "Set.isSupsetOf" $
    prop "Set.isSupsetOf xs == Set.null . (flip Set.difference) xs" $
      \xs ys ->
        S.isSupsetOf (xs :: S.Set Int) (ys :: S.Set Int)
          == S.null (S.difference ys xs)

  describe "Set.smallestElem" $ do
    it "invalid: Set.smallestElem Set.empty == Nothing" $
      isNothing $ S.smallestElem S.empty
    prop "valid: Set.smallestElem == Just . minimum . Set.toList" $
      \xs ->
        not (S.null xs)
          ==> (S.smallestElem xs :: Maybe Int)
            == (Just . minimum $ S.toList xs)

  describe "Set.largestElem" $ do
    it "invalid: Set.largestElem Set.empty == Nothing" $
      isNothing $ S.largestElem S.empty
    prop "valid: Set.largestElem == Just . maxmimum . Set.toList" $
      \xs ->
        not (S.null xs)
          ==> (S.largestElem xs :: Maybe Int)
            == (Just . maximum $ S.toList xs)

  describe "Set.kthSmallestElem" $ do
    prop "invalid: Set.kthSmallestElem k == Nothing" $
      \xs ->
        forAll (choose (S.size xs + 1, S.size xs + 2)) $
          \k -> isNothing (S.kthSmallestElem k xs :: Maybe Int)
    prop
      "valid: Set.kthSmallestElem k == Just . (!! (k - 1)) . sort . Set.toList"
      $ \xs ->
        not (null xs)
          ==> forAll (choose (1, S.size xs))
          $ \k ->
            (S.kthSmallestElem k xs :: Maybe Int)
              == (Just . (!! (k - 1)) . List.sort $ S.toList xs)

  describe "Set.kthLargestElem" $ do
    prop "invalid: Set.kthLargestElem k == Nothing" $
      \xs ->
        forAll (choose (S.size xs + 1, S.size xs + 2)) $
          \k -> isNothing (S.kthLargestElem k xs :: Maybe Int)
    prop
      "valid: Set.kthLargestElem k == Just . (!! (k - 1)) . reverse . sort . Set.toList"
      $ \xs ->
        not (null xs)
          ==> forAll (choose (1, S.size xs))
          $ \k ->
            (S.kthLargestElem k xs :: Maybe Int)
              == (Just . (!! (k - 1)) . reverse . List.sort $ S.toList xs)
