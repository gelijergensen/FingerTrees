module MultiSetSpec where

import Control.Exception (evaluate)
import qualified Data.Bifunctor as Bifunc
import qualified Data.IntMap.Strict as Map
import qualified Data.List as List
import Data.Maybe (isNothing, listToMaybe)
import qualified MultiSet as MS
import qualified Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (MS.MultiSet a) where
  arbitrary = fmap MS.fromList arbitrary

spec :: Spec
spec = do
  describe "MultiSet.null/MultiSet.empty" $ do
    it "MultiSet.null MultiSet.empty == True" $ do
      MS.null MS.empty `shouldBe` True

  describe "MultiSet.null/MultiSet.singleton" $ do
    prop "MultiSet.null . MultiSet.singleton == const False" $ do
      \x -> not $ MS.null (MS.singleton x :: MS.MultiSet Int)

  describe "MultiSet.size" $ do
    prop "MultiSet.size == length . MultiSet.toList" $ do
      \xs -> MS.size (xs :: MS.MultiSet Int) == length (MS.toList xs)

  describe "MultiSet.numUniqueElems" $ do
    prop "MultiSet.numUniqueElems == length . nub . MultiSet.toList" $
      \xs ->
        MS.numUniqueElems (xs :: MS.MultiSet Int)
          == length (List.nub (MS.toList xs))

  describe "MultiSet.toList" $ do
    prop "MultiSet.toList . MultiSet.fromList ==  sort" $
      \xs -> (List.sort xs :: [Int]) == (MS.toList . MS.fromList $ xs)
    prop "MultiSet.fromList . MultiSet.toList == id" $
      \xs -> (xs :: MS.MultiSet Int) == (MS.fromList . MS.toList $ xs)

  describe "MultiSet.fromList/MultiSet.fromFoldable" $ do
    prop "MultiSet.fromList == MultiSet.fromFoldable" $
      \xs -> MS.fromList (xs :: [Int]) == MS.fromFoldable xs

  describe "MultiSet.fromAscList/MultiSet.fromAscFoldable" $ do
    prop "MultiSet.fromAscList . sort == MultiSet.fromAscFoldable . sort" $
      \xs ->
        MS.fromAscList (List.sort xs :: [Int])
          == MS.fromAscFoldable (List.sort xs)

  describe "MultiSet.fromDistinctAscList/MultiSet.fromDistinctAscFoldable" $ do
    prop
      "MultiSet.fromDistinctAscList . nub . sort == MultiSet.fromDistinctAscFoldable . nub . sort"
      $ \xs ->
        (MS.fromDistinctAscList . List.nub . List.sort $ (xs :: [Int]))
          == (MS.fromDistinctAscFoldable . List.nub . List.sort $ xs)

  describe "MultiSet.fromDescList/MultiSet.fromDescFoldable" $ do
    prop
      "MultiSet.fromDescList . reverse . sort == MultiSet.fromDescFoldable . reverse . sort"
      $ \xs ->
        (MS.fromDescList . reverse . List.sort $ (xs :: [Int]))
          == (MS.fromDescFoldable . reverse . List.sort $ xs)

  describe "MultiSet.fromDistinctDescList/MultiSet.fromDistinctDescFoldable" $ do
    prop
      "MultiSet.fromDistinctDescList . reverse . nub . sort == MultiSet.fromDistinctDescFoldable . reverse . nub . sort"
      $ \xs ->
        ( MS.fromDistinctDescList . reverse . List.nub . List.sort $
            (xs :: [Int])
        )
          == (MS.fromDistinctDescFoldable . reverse . List.nub . List.sort $ xs)

  describe "MultiSet.fromAscList" $ do
    prop "MultiSet.fromAscList . sort == MultiSet.fromList" $
      \xs ->
        (MS.fromAscList . List.sort $ (xs :: [Int])) == MS.fromList xs

  describe "MultiSet.fromDistinctAscList" $ do
    prop "MultiSet.fromDistinctAscList . sort == MultiSet.fromList . nub" $
      \xs ->
        (MS.fromDistinctAscList . List.nub . List.sort $ (xs :: [Int])) == MS.fromList (List.nub xs)

  describe "MultiSet.fromDescList" $ do
    prop "MultiSet.fromDescList . reverse . sort == MultiSet.fromList" $
      \xs ->
        (MS.fromDescList . reverse . List.sort $ (xs :: [Int]))
          == MS.fromList xs

  describe "MultiSet.fromDistinctDescList" $ do
    prop
      "MultiSet.fromDistinctDescList . reverse . nub . sort == MultiSet.fromList . nub "
      $ \xs ->
        (MS.fromDistinctDescList . reverse . List.nub . List.sort $ (xs :: [Int]))
          == MS.fromList (List.nub xs)

  describe "MultiSet.insert" $ do
    prop "MultiSet.toList . MultiSet.insert x == sort . (x :) . MultiSet.toList" $
      \x xs ->
        MS.toList (MS.insert x xs :: MS.MultiSet Int)
          == (List.sort . (x :) . MS.toList $ xs)

  describe "MultiSet.deleteOnce" $ do
    prop
      "MultiSet.toList . MultiSet.deleteOnce x == delete x . MultiSet.toList"
      $ \x xs ->
        MS.toList (MS.deleteOnce x xs :: MS.MultiSet Int)
          == List.delete x (MS.toList xs)

  describe "MultiSet.deleteEach" $ do
    prop
      "MultiSet.toList . MultiSet.deleteEach x == filter (/= x) . MultiSet.toList"
      $ \x xs ->
        MS.toList (MS.deleteEach x xs :: MS.MultiSet Int)
          == List.filter (/= x) (MS.toList xs)

  describe "MultiSet.count" $ do
    prop "MultiSet.count x == length . elemIndices x . MultiSet.toList" $
      \x xs ->
        MS.count x (xs :: MS.MultiSet Int)
          == length (List.elemIndices x (MS.toList xs))

  describe "MultiSet.map/MultiSet.mapMonotonic" $ do
    prop "MultiSet.map f == MultiSet.mapMonotonic f for monotonic functions" $
      \xs -> (MS.map (* 2) xs :: MS.MultiSet Int) == MS.mapMonotonic (* 2) xs

  describe "MultiSet.map" $ do
    prop "MultiSet.toList . MultiSet.map f == map f . MultiSet.toList" $
      \xs -> MS.toList (MS.map (* 2) xs :: MS.MultiSet Int) == map (* 2) (MS.toList xs)

  describe "MultiSet.union" $ do
    prop
      "MultiSet.toList (xs `MultiSet.union` ys) == sort $ MultiSet.toList xs ++ MultiSet.toList ys"
      $ \xs ys ->
        (MS.toList (xs `MS.union` ys) :: [Int])
          == List.sort (MS.toList xs ++ MS.toList ys)

  describe "MultiSet.intersection" $ do
    prop
      "MultiSet.toList (xs `MultiSet.intersection` ys) == unWithCounts . Map.toList . Map.intersectionWith min (asMap xs) $ asMap ys"
      $ \xs ys ->
        let asMap zs =
              Map.fromAscList
                . fmap (\z -> (head z, length z))
                . List.group
                . List.sort
                $ (MS.toList zs :: [Int])
            unWithCounts = concatMap (uncurry (flip replicate))
         in (MS.toList (xs `MS.intersection` ys) :: [Int])
              == ( unWithCounts
                     . Map.toList
                     . Map.intersectionWith min (asMap xs)
                     $ asMap ys
                 )

  describe "MultiSet.difference" $ do
    prop
      "MultiSet.toList (xs `MultiSet.difference` ys) == sort $ MultiSet.toList xs \\ MultiSet.toList ys"
      $ \xs ys ->
        (MS.toList (xs `MS.difference` ys) :: [Int])
          == List.sort (MS.toList xs List.\\ MS.toList ys)

  describe "MultiSet.areDisjoint" $ do
    prop "MultiSet.areDisjoint xs == MultiSet.null . MultiSet.intersection xs" $
      \xs ys ->
        MS.areDisjoint (xs :: MS.MultiSet Int) (ys :: MS.MultiSet Int)
          == MS.null (MS.intersection xs ys)

  describe "MultiSet.isSubsetOf" $
    prop "MultiSet.isSubsetOf xs == MultiSet.null . MultiSet.difference xs" $
      \xs ys ->
        MS.isSubsetOf (xs :: MS.MultiSet Int) (ys :: MS.MultiSet Int)
          == MS.null (MS.difference xs ys)

  describe "MultiSet.isSupsetOf" $
    prop
      "MultiSet.isSupsetOf xs == MultiSet.null . (flip MultiSet.difference) xs"
      $ \xs ys ->
        MS.isSupsetOf (xs :: MS.MultiSet Int) (ys :: MS.MultiSet Int)
          == MS.null (MS.difference ys xs)

  describe "MultiSet.support" $
    prop
      "MultiSet.toList . MultiSet.support == nub . MultiSet.toList"
      $ \xs ->
        Set.toList (MS.support (xs :: MS.MultiSet Int))
          == List.nub (MS.toList xs)

  describe "MultiSet.smallestElem" $ do
    it "invalid: MultiSet.smallestElem MultiSet.empty == Nothing" $
      isNothing $ MS.smallestElem MS.empty
    prop "valid: MultiSet.smallestElem == Just . minimum . MultiSet.toList" $
      \xs ->
        not (MS.null xs)
          ==> (MS.smallestElem xs :: Maybe Int)
            == (Just . minimum $ MS.toList xs)

  describe "MultiSet.largestElem" $ do
    it "invalid: MultiSet.largestElem MultiSet.empty == Nothing" $
      isNothing $ MS.largestElem MS.empty
    prop "valid: MultiSet.largestElem == Just . maxmimum . MultiSet.toList" $
      \xs ->
        not (MS.null xs)
          ==> (MS.largestElem xs :: Maybe Int)
            == (Just . maximum $ MS.toList xs)

  describe "MultiSet.kthSmallestElem" $ do
    prop "invalid: MultiSet.kthSmallestElem k == Nothing" $
      \xs ->
        forAll (choose (MS.size xs + 1, MS.size xs + 2)) $
          \k -> isNothing (MS.kthSmallestElem k xs :: Maybe Int)
    prop
      "valid: MultiSet.kthSmallestElem k == Just . (!! (k - 1)) . sort . MultiSet.toList"
      $ \xs ->
        not (null xs)
          ==> forAll (choose (1, MS.size xs))
          $ \k ->
            (MS.kthSmallestElem k xs :: Maybe Int)
              == (Just . (!! (k - 1)) . List.sort $ MS.toList xs)

  describe "MultiSet.kthLargestElem" $ do
    prop "invalid: MultiSet.kthLargestElem k == Nothing" $
      \xs ->
        forAll (choose (MS.size xs + 1, MS.size xs + 2)) $
          \k -> isNothing (MS.kthLargestElem k xs :: Maybe Int)
    prop
      "valid: MultiSet.kthLargestElem k == Just . (!! (k - 1)) . reverse . sort . MultiSet.toList"
      $ \xs ->
        not (null xs)
          ==> forAll (choose (1, MS.size xs))
          $ \k ->
            (MS.kthLargestElem k xs :: Maybe Int)
              == (Just . (!! (k - 1)) . reverse . List.sort $ MS.toList xs)
