module OrdSeqSpec where

import Control.Exception (evaluate)
import qualified Data.Bifunctor as Bifunc
import qualified Data.IntMap.Strict as Map
import qualified Data.List as List
import Data.Maybe (isNothing, listToMaybe)
import qualified OrdSeq as OS
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (OS.OrdSeq a) where
  arbitrary = fmap OS.fromList arbitrary

spec :: Spec
spec = do
  describe "OrdSeq.null/OrdSeq.empty" $ do
    it "OrdSeq.null OrdSeq.empty == True" $ do
      OS.null OS.empty `shouldBe` True

  describe "OrdSeq.null/OrdSeq.singleton" $ do
    prop "OrdSeq.null . OrdSeq.singleton == const False" $ do
      \x -> not $ OS.null (OS.singleton x :: OS.OrdSeq Int)

  describe "OrdSeq.size" $ do
    prop "OrdSeq.size == length . OrdSeq.toList" $ do
      \xs -> OS.size (xs :: OS.OrdSeq Int) == length (OS.toList xs)

  describe "OrdSeq.fromList / OrdSeq.fromFoldable" $ do
    prop "OrdSeq.fromList == OrdSeq.fromFoldable" $
      \xs -> OS.fromList (xs :: [Int]) == OS.fromFoldable xs

  describe "OrdSeq.fromAscList / OrdSeq.fromAscFoldable" $ do
    prop "OrdSeq.fromAscList == OrdSeq.fromAscFoldable" $
      \xs -> OS.fromAscList (xs :: [Int]) == OS.fromAscFoldable xs

  describe "OrdSeq.fromDescList / OrdSeq.fromDescFoldable" $ do
    prop "OrdSeq.fromDescList == OrdSeq.fromDescFoldable" $
      \xs -> OS.fromDescList (xs :: [Int]) == OS.fromDescFoldable xs

  describe "OrdSeq.fromAscList" $ do
    prop "OrdSeq.fromAscList . sort == OrdSeq.fromList" $
      \xs ->
        OS.fromAscList (List.sort xs :: [Int]) == OS.fromList xs

  describe "OrdSeq.fromDescList" $ do
    prop "OrdSeq.fromDescList . reverse . sort == OrdSeq.fromList" $
      \xs -> OS.fromDescList (reverse (List.sort xs :: [Int])) == OS.fromList xs

  describe "OrdSeq.insert" $ do
    prop "OrdSeq.toList . OrdSeq.insert x == sort . (x :) . OrdSeq.toList" $
      \x xs ->
        OS.toList (OS.insert x xs :: OS.OrdSeq Int)
          == (List.sort . (x :) . OS.toList $ xs)

  describe "OrdSeq.delete" $ do
    prop "OrdSeq.toList . OrdSeq.delete x == delete x . OrdSeq.toList" $
      \x xs ->
        OS.toList (OS.delete x xs :: OS.OrdSeq Int)
          == List.delete x (OS.toList xs)

  describe "OrdSeq.member" $ do
    prop "OrdSeq.member x == elem x . OrdSeq.toList" $
      \x xs -> OS.member x (xs :: OS.OrdSeq Int) == (x `elem` OS.toList xs)

  describe "OrdSeq.map/OrdSeq.mapMonotonic" $ do
    prop "OrdSeq.map f == OrdSeq.mapMonotonic f for monotonic functions" $
      \xs -> (OS.map (* 2) xs :: OS.OrdSeq Int) == OS.mapMonotonic (* 2) xs

  describe "OrdSeq.map" $ do
    prop "OrdSeq.toList . OrdSeq.map f == map f . OrdSeq.toList" $
      \xs ->
        OS.toList (OS.map (* 2) xs :: OS.OrdSeq Int)
          == map (* 2) (OS.toList xs)

  describe "OrdSeq.><" $ do
    prop
      "OrdSeq.toList (xs OrdSeq.>< ys) == sort $ OrdSeq.toList xs ++ OrdSeq.toList ys"
      $ \xs ys ->
        (OS.toList (xs OS.>< ys) :: [Int])
          == List.sort (OS.toList xs ++ OS.toList ys)

  describe "OrdSeq.head" $ do
    it "throws an exception on the empty sequence" $
      evaluate (OS.head OS.empty)
        `shouldThrow` errorCall "OrdSeq.head: empty OrdSeq"
    prop "OrdSeq.head == head . OrdSeq.toList" $
      \xs -> not (null xs) ==> (OS.head xs :: Int) == head (OS.toList xs)

  describe "OrdSeq.tail" $ do
    it "throws an exception on the empty sequence" $
      evaluate (OS.tail OS.empty)
        `shouldThrow` errorCall "OrdSeq.tail: empty OrdSeq"
    prop "OrdSeq.toList . OrdSeq.tail == tail . OrdSeq.toList" $
      \xs ->
        not (null xs)
          ==> OS.toList (OS.tail xs :: OS.OrdSeq Int) == tail (OS.toList xs)

  describe "OrdSeq.last" $ do
    it "throws an exception on the empty sequence" $
      evaluate (OS.last OS.empty)
        `shouldThrow` errorCall "OrdSeq.last: empty OrdSeq"
    prop "OrdSeq.last == last . OrdSeq.toList" $
      \xs -> not (null xs) ==> (OS.last xs :: Int) == last (OS.toList xs)

  describe "OrdSeq.init" $ do
    it "throws an exception on the empty sequence" $
      evaluate (OS.init OS.empty)
        `shouldThrow` errorCall "OrdSeq.init: empty OrdSeq"
    prop "OrdSeq.toList . OrdSeq.init == init . OrdSeq.toList" $
      \xs ->
        not (null xs)
          ==> OS.toList (OS.init xs :: OS.OrdSeq Int) == init (OS.toList xs)

  describe "OrdSeq.lookup" $ do
    prop "invalid: OrdSeq.lookup i == Nothing" $
      \xs -> forAll (choose (length xs, length xs + 2)) $
        \i -> isNothing (OS.lookup i xs :: Maybe Int)
    prop "valid: OrdSeq.lookup i == Just . (!! i) . OrdSeq.toList" $
      \xs ->
        not (null xs)
          ==> forAll (choose (0, length xs - 1))
          $ \i -> (OS.lookup i xs :: Maybe Int) == Just (OS.toList xs !! i)

  describe "OrdSeq.!?" $ do
    prop "OrdSeq.!!? == flip OrdSeq.lookup" $
      \xs -> forAll (choose (-1, length xs + 1)) $
        \i -> (xs OS.!? i :: Maybe Int) == OS.lookup i xs

  describe "OrdSeq.index" $ do
    prop "OrdSeq.index i == (!! i) . OrdSeq.toList" $
      \xs ->
        not (null xs)
          ==> forAll (choose (0, length xs - 1))
          $ \i -> (OS.index xs i :: Int) == OS.toList xs !! i

  describe "OrdSeq.drop" $ do
    prop "OrdSeq.drop i == snd . OrdSeq.splitAt i" $
      \xs -> forAll (choose (-1, length xs)) $
        \i -> (OS.drop i xs :: OS.OrdSeq Int) == (snd . OS.splitAt i $ xs)

  describe "OrdSeq.take" $ do
    prop "OrdSeq.take i == fst . OrdSeq.splitAt i" $
      \xs -> forAll (choose (-1, length xs)) $
        \i -> (OS.take i xs :: OS.OrdSeq Int) == (fst . OS.splitAt i $ xs)

  describe "OrdSeq.splitAt" $ do
    prop
      "bimap OrdSeq.toList OrdSeq.toList . OrdSeq.splitAt i == splitAt i . OrdSeq.toList"
      $ \xs -> forAll (choose (-1, length xs)) $
        \i ->
          Bifunc.bimap OS.toList OS.toList (OS.splitAt i (xs :: OS.OrdSeq Int))
            == splitAt i (OS.toList xs)

  describe "OrdSeq.foldlWithIndex" $ do
    prop
      "OrdSeq.foldlWithIndex and OrdSeq.foldrWithIndex agree for commutative functions"
      $ \xs ->
        OS.foldlWithIndex (\b i a -> b + i * a) 0 (xs :: OS.OrdSeq Int)
          == OS.foldrWithIndex (\i a b -> b + i * a) 0 xs

  describe "OrdSeq.foldrWithIndex" $ do
    prop "OrdSeq.foldrWithIndex is a foldr with an index" $
      \xs ->
        OS.foldrWithIndex (\i a b -> b + i * a) 0 (xs :: OS.OrdSeq Int)
          == foldr (\(i, a) b -> b + i * a) 0 (zip [0, 1 ..] (OS.toList xs))

  describe "OrdSeq.findIndicesL" $ do
    prop "OrdSeq.findIndicesL p == findIndices p . OrdSeq.toList" $
      \xs ->
        OS.findIndicesL even (xs :: OS.OrdSeq Int)
          == List.findIndices even (OS.toList xs)

  describe "OrdSeq.findIndexL" $ do
    prop "OrdSeq.findIndexL p == findIndex p . OrdSeq.toList" $
      \xs ->
        OS.findIndexL even (xs :: OS.OrdSeq Int)
          == List.findIndex even (OS.toList xs)

  describe "OrdSeq.findIndicesR" $ do
    prop "OrdSeq.findIndicesR p == reverse . OrdSeq.findIndicesL p" $
      \xs ->
        OS.findIndicesR even (xs :: OS.OrdSeq Int)
          == reverse (OS.findIndicesL even xs)

  describe "OrdSeq.findIndexR" $ do
    prop "OrdSeq.findIndexR p == listToMaybe . reverse . OS.findIndicesL p" $
      \xs ->
        OS.findIndexR even (xs :: OS.OrdSeq Int)
          == (listToMaybe . reverse . OS.findIndicesL even $ xs)

  describe "OrdSeq.elemIndicesL" $ do
    prop "OrdSeq.elemIndicesL p == elemIndices p . OrdSeq.toList" $
      \x xs ->
        OS.elemIndicesL x (xs :: OS.OrdSeq Int)
          == List.elemIndices x (OS.toList xs)

  describe "OrdSeq.elemIndexL" $ do
    prop "OrdSeq.elemIndexL p == elemIndex p . OrdSeq.toList" $
      \x xs ->
        OS.elemIndexL x (xs :: OS.OrdSeq Int)
          == List.elemIndex x (OS.toList xs)

  describe "OrdSeq.elemIndicesR" $ do
    prop "OrdSeq.elemIndicesR p == reverse . OrdSeq.elemIndicesL p" $
      \x xs ->
        OS.elemIndicesR x (xs :: OS.OrdSeq Int)
          == reverse (OS.elemIndicesL x xs)

  describe "OrdSeq.elemIndexR" $ do
    prop "OrdSeq.elemIndexR p == listToMaybe . reverse . OS.elemIndicesL p" $
      \x xs ->
        OS.elemIndexR x (xs :: OS.OrdSeq Int)
          == (listToMaybe . reverse . OS.elemIndicesL x $ xs)

  describe "OrdSeq.breakl" $ do
    prop
      "bimap OrdSeq.toList OrdSeq.toList . OrdSeq.breakl p == break p . OrdSeq.toList"
      $ \xs ->
        Bifunc.bimap OS.toList OS.toList (OS.breakl even (xs :: OS.OrdSeq Int))
          == break even (OS.toList xs)

  describe "OrdSeq.breakr" $ do
    prop
      "bimap OrdSeq.toList OrdSeq.toList . OrdSeq.breakr p == (\\(ys, zs) -> (reverse zs, reverse ys)) . break p . reverse . OrdSeq.toList"
      $ \xs ->
        Bifunc.bimap OS.toList OS.toList (OS.breakr even (xs :: OS.OrdSeq Int))
          == ( (\(ys, zs) -> (reverse zs, reverse ys))
                 . break even
                 . reverse
                 $ OS.toList xs
             )

  describe "OrdSeq.spanl" $ do
    prop
      "bimap OrdSeq.toList OrdSeq.toList . OrdSeq.spanl p == span p . OrdSeq.toList"
      $ \xs ->
        Bifunc.bimap OS.toList OS.toList (OS.spanl even (xs :: OS.OrdSeq Int))
          == span even (OS.toList xs)

  describe "OrdSeq.spanr" $ do
    prop
      "bimap OrdSeq.toList OrdSeq.toList . OrdSeq.spanr p == (\\(ys, zs) -> (reverse zs, reverse ys)) . span p . reverse . OrdSeq.toList"
      $ \xs ->
        Bifunc.bimap OS.toList OS.toList (OS.spanr even (xs :: OS.OrdSeq Int))
          == ( (\(ys, zs) -> (reverse zs, reverse ys))
                 . span even
                 . reverse
                 $ OS.toList xs
             )

  describe "OrdSeq.dropWhileL" $ do
    prop "OrdSeq.dropWhileL p == snd . OrdSeq.spanl p" $
      \xs -> OS.dropWhileL even (xs :: OS.OrdSeq Int) == snd (OS.spanl even xs)

  describe "OrdSeq.takeWhileL" $ do
    prop "OrdSeq.takeWhileL p == fst . OrdSeq.spanl p" $
      \xs -> OS.takeWhileL even (xs :: OS.OrdSeq Int) == fst (OS.spanl even xs)

  describe "OrdSeq.dropWhileR" $ do
    prop "OrdSeq.dropWhileR p == fst . OrdSeq.spanr p" $
      \xs -> OS.dropWhileR even (xs :: OS.OrdSeq Int) == fst (OS.spanr even xs)

  describe "OrdSeq.takeWhileR" $ do
    prop "OrdSeq.takeWhileR p == snd . OrdSeq.spanr p" $
      \xs -> OS.takeWhileR even (xs :: OS.OrdSeq Int) == snd (OS.spanr even xs)

  describe "OrdSeq.partition" $ do
    prop
      "bimap OrdSeq.toList OrdSeq.toList . OrdSeq.partition p == partition p . OrdSeq.toList"
      $ \xs ->
        Bifunc.bimap OS.toList OS.toList (OS.partition even (xs :: OS.OrdSeq Int))
          == List.partition even (OS.toList xs)

  describe "OrdSeq.filter" $ do
    prop "OrdSeq.toList . OrdSeq.filter p == filter p . OrdSeq.toList" $
      \xs ->
        OS.toList (OS.filter even (xs :: OS.OrdSeq Int))
          == List.filter even (OS.toList xs)

  describe "OrdSeq.zip" $ do
    prop "OrdSeq.toList . OrdSeq.zip xs == zip (OrdSeq.toList xs) . OrdSeq.toList" $
      \xs ys ->
        OS.toList (OS.zip xs ys :: OS.OrdSeq (Int, Int))
          == zip (OS.toList xs) (OS.toList ys)

  describe "OrdSeq.zipWith" $ do
    prop
      "OrdSeq.toList . OrdSeq.zipWith f xs == zipWith f (OrdSeq.toList xs) . OrdSeq.toList"
      $ \xs ys ->
        OS.toList (OS.zipWith (+) xs ys :: OS.OrdSeq Int)
          == zipWith (+) (OS.toList xs) (OS.toList ys)

  describe "OrdSeq.unzip" $ do
    prop
      "bimap OrdSeq.toList OrdSeq.toList . OrdSeq.unzip == unzip . OrdSeq.toList"
      $ \xs ->
        Bifunc.bimap OS.toList OS.toList (OS.unzip (xs :: OS.OrdSeq (Int, Int)))
          == unzip (OS.toList xs)
