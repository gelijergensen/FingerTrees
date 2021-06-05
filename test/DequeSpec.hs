module DequeSpec where

import Control.Exception (evaluate)
import qualified Data.Bifunctor as Bifunc
import qualified Data.List as List
import Data.Maybe (isNothing, listToMaybe)
import qualified Deque as D
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary a => Arbitrary (D.Deque a) where
  arbitrary = fmap D.fromList arbitrary

spec :: Spec
spec = do
  describe "Deque.empty" $ do
    it "Deque.empty == Deque.Empty" $ do
      (D.empty :: D.Deque Int) `shouldBe` D.Empty

  describe "Deque.singleton" $ do
    prop "Deque.singleton == (Deque.:<| Deque.empty)" $
      \x -> (D.singleton x :: D.Deque Int) == x D.:<| D.empty
    prop "Deque.singleton == (Deque.empty Deque.:|>)" $
      \x -> (D.singleton x :: D.Deque Int) == D.empty D.:|> x

  describe "Deque.null" $ do
    it "Deque.null Deque.empty == True" $ do
      D.null D.empty `shouldBe` True
    prop "Deque.null . Deque.singleton == const False" $
      \x -> not $ D.null (D.singleton x :: D.Deque Int)

  describe "Deque.length" $ do
    prop "Deque.length == length . Deque.toList" $
      \xs -> D.length (xs :: D.Deque Int) == length (D.toList xs)

  describe "Deque.toList" $ do
    prop "Deque.toList . Deque.fromList == id" $
      \xs -> (xs :: [Int]) == (D.toList . D.fromList $ xs)
    prop "Deque.fromList . Deque.toList == id" $
      \xs -> (xs :: D.Deque Int) == (D.fromList . D.toList $ xs)

  describe "Deque.fromList / Deque.fromFoldable" $ do
    prop "Deque.fromList == Deque.fromFoldable" $
      \xs -> D.fromList (xs :: [Int]) == D.fromFoldable xs

  describe "Deque.><" $ do
    prop "Deque.toList (xs Deque.>< ys) == Deque.toList xs ++ Deque.toList ys" $
      \xs ys -> (D.toList (xs D.>< ys) :: [Int]) == (D.toList xs ++ D.toList ys)

  describe "Deque.head" $ do
    it "throws an exception on the empty deque" $
      evaluate (D.head D.empty) `shouldThrow` errorCall "Deque.head: empty Deque"
    prop "Deque.head == head . Deque.toList" $
      \xs -> not (null xs) ==> (D.head xs :: Int) == head (D.toList xs)
    prop "doesn't evaluate the deque fully" $
      \x -> D.head (D.fromList [x :: Int, undefined]) == x

  describe "Deque.tail" $ do
    it "throws an exception on the empty deque" $
      evaluate (D.tail D.empty) `shouldThrow` errorCall "Deque.tail: empty Deque"
    prop "Deque.toList . Deque.tail == tail . Deque.toList" $
      \xs ->
        not (null xs)
          ==> D.toList (D.tail xs :: D.Deque Int) == tail (D.toList xs)

  describe "Deque.last" $ do
    it "throws an exception on the empty deque" $
      evaluate (D.last D.empty) `shouldThrow` errorCall "Deque.last: empty Deque"
    prop "Deque.last == last . Deque.toList" $
      \xs -> not (null xs) ==> (D.last xs :: Int) == last (D.toList xs)
    prop "doesn't evaluate the deque fully" $
      \x -> D.last (D.fromList [undefined, x :: Int]) == x

  describe "Deque.init" $ do
    it "throws an exception on the empty deque" $
      evaluate (D.init D.empty) `shouldThrow` errorCall "Deque.init: empty Deque"
    prop "Deque.toList . Deque.init == init . Deque.toList" $
      \xs ->
        not (null xs)
          ==> D.toList (D.init xs :: D.Deque Int) == init (D.toList xs)

  describe "Deque.lookup" $ do
    prop "invalid: Deque.lookup i == Nothing" $
      \xs -> forAll (choose (length xs, length xs + 2)) $
        \i -> isNothing (D.lookup i xs :: Maybe Int)
    prop "valid: Deque.lookup i == Just . (!! i) . Deque.toList" $
      \xs ->
        not (null xs)
          ==> forAll (choose (0, length xs - 1))
          $ \i -> (D.lookup i xs :: Maybe Int) == Just (D.toList xs !! i)

  describe "Deque.!?" $ do
    prop "Deque.!!? == flip Deque.lookup" $
      \xs -> forAll (choose (-1, length xs + 1)) $
        \i -> (xs D.!? i :: Maybe Int) == D.lookup i xs

  describe "Deque.index" $ do
    prop "Deque.index i == (!! i) . Deque.toList" $
      \xs ->
        not (null xs)
          ==> forAll (choose (0, length xs - 1))
          $ \i -> (D.index xs i :: Int) == D.toList xs !! i

  describe "Deque.adjustAt" $ do
    prop "Deque.adjustAt i modifies precisely the ith position" $
      \x xs -> forAll (choose (-1, length xs + 1)) $
        \i ->
          D.lookup i (D.adjustAt i (+ (x :: Int)) xs :: D.Deque Int)
            == fmap (+ x) (D.lookup i xs)

  describe "Deque.insertAt" $ do
    prop "Deque.insertAt i x inserts x at position i" $
      \x xs -> forAll (choose (-1, length xs + 1)) $
        \i ->
          D.lookup i (D.insertAt i x xs :: D.Deque Int)
            == if i `elem` [-1, length xs + 1] then Nothing else Just x

  describe "Deque.deleteAt" $ do
    prop "Deque.deleteAt i removes the ith element" $
      \xs -> forAll (choose (-1, length xs + 1)) $
        \i ->
          D.lookup i (D.deleteAt i xs :: D.Deque Int)
            == if i == (-1) then Nothing else D.lookup (i + 1) xs

  describe "Deque.updateAt" $ do
    prop "Deque.updateAt i x replaces the ith element with x" $
      \x xs -> forAll (choose (-1, length xs)) $
        \i ->
          D.lookup i (D.updateAt i x xs :: D.Deque Int)
            == if i `elem` [-1, length xs] then Nothing else Just x

  describe "Deque.drop" $ do
    prop "Deque.drop i == snd . Deque.splitAt i" $
      \xs -> forAll (choose (-1, length xs)) $
        \i -> (D.drop i xs :: D.Deque Int) == (snd . D.splitAt i $ xs)

  describe "Deque.take" $ do
    prop "Deque.take i == fst . Deque.splitAt i" $
      \xs -> forAll (choose (-1, length xs)) $
        \i -> (D.take i xs :: D.Deque Int) == (fst . D.splitAt i $ xs)

  describe "Deque.splitAt" $ do
    prop
      "bimap Deque.toList Deque.toList . Deque.splitAt i == splitAt i . Deque.toList"
      $ \xs -> forAll (choose (-1, length xs)) $
        \i ->
          Bifunc.bimap D.toList D.toList (D.splitAt i (xs :: D.Deque Int))
            == splitAt i (D.toList xs)

  describe "Deque.foldMapWithIndex" $ do
    prop "Deque.foldMapWithIndex is a fold map with index" $
      \xs ->
        D.foldMapWithIndex replicate (xs :: D.Deque Char)
          == concat (zipWith replicate [0, 1 ..] (D.toList xs))

  describe "Deque.foldlWithIndex" $ do
    prop
      "Deque.foldlWithIndex and Deque.foldrWithIndex agree for commutative functions"
      $ \xs ->
        D.foldlWithIndex (\b i a -> b + i * a) 0 (xs :: D.Deque Int)
          == D.foldrWithIndex (\i a b -> b + i * a) 0 xs

  describe "Deque.foldrWithIndex" $ do
    prop "Deque.foldrWithIndex is a foldr with an index" $
      \xs ->
        D.foldrWithIndex (\i a b -> b + i * a) 0 (xs :: D.Deque Int)
          == foldr (\(i, a) b -> b + i * a) 0 (zip [0, 1 ..] (D.toList xs))

  describe "Deque.mapWithIndex" $ do
    prop "Deque.mapWithIndex is a map with an index" $
      \xs ->
        D.toList (D.mapWithIndex (*) (xs :: D.Deque Int))
          == zipWith (*) [0, 1 ..] (D.toList xs)

  describe "Deque.traverseWithIndex" $ do
    prop "Deque.traverseWithIndex is a traversal with an index" $
      \xs ->
        map D.toList (D.traverseWithIndex replicate (xs :: D.Deque Int))
          == traverse (uncurry replicate) (zip [0, 1 ..] (D.toList xs))

  describe "Deque.scanl" $ do
    prop "Deque.toList . Deque.scanl f z == scanl f z . Deque.toList" $
      \xs ->
        D.toList (D.scanl (+) 0 (xs :: D.Deque Int))
          == scanl (+) 0 (D.toList xs)

  describe "Deque.scanl1" $ do
    it "throws an exception on the empty deque" $
      evaluate (D.scanl1 (+) D.empty)
        `shouldThrow` errorCall "Empty deque encountered in call to Deque.scanl1"
    prop "Deque.toList . Deque.scanl1 f == scanl1 f . Deque.toList" $
      \xs ->
        not (null xs)
          ==> D.toList (D.scanl1 (+) (xs :: D.Deque Int))
          == scanl1 (+) (D.toList xs)

  describe "Deque.scanr" $ do
    prop "Deque.toList . Deque.scanr f z == scanr f z . Deque.toList" $
      \xs ->
        D.toList (D.scanr (+) 0 (xs :: D.Deque Int))
          == scanr (+) 0 (D.toList xs)

  describe "Deque.scanr1" $ do
    it "throws an exception on the empty deque" $
      evaluate (D.scanr1 (+) D.empty)
        `shouldThrow` errorCall "Empty deque encountered in call to Deque.scanr1"
    prop "Deque.toList . Deque.scanr1 f == scanr1 f . Deque.toList" $
      \xs ->
        not (null xs)
          ==> D.toList (D.scanr1 (+) (xs :: D.Deque Int))
          == scanr1 (+) (D.toList xs)

  describe "Deque.findIndicesL" $ do
    prop "Deque.findIndicesL p == findIndices p . Deque.toList" $
      \xs ->
        D.findIndicesL even (xs :: D.Deque Int)
          == List.findIndices even (D.toList xs)

  describe "Deque.findIndexL" $ do
    prop "Deque.findIndexL p == findIndex p . Deque.toList" $
      \xs ->
        D.findIndexL even (xs :: D.Deque Int)
          == List.findIndex even (D.toList xs)

  describe "Deque.findIndicesR" $ do
    prop "Deque.findIndicesR p == reverse . Deque.findIndicesL p" $
      \xs ->
        D.findIndicesR even (xs :: D.Deque Int)
          == reverse (D.findIndicesL even xs)

  describe "Deque.findIndexR" $ do
    prop "Deque.findIndexR p == listToMaybe . reverse . D.findIndicesL p" $
      \xs ->
        D.findIndexR even (xs :: D.Deque Int)
          == (listToMaybe . reverse . D.findIndicesL even $ xs)

  describe "Deque.elemIndicesL" $ do
    prop "Deque.elemIndicesL p == elemIndices p . Deque.toList" $
      \x xs ->
        D.elemIndicesL x (xs :: D.Deque Int)
          == List.elemIndices x (D.toList xs)

  describe "Deque.elemIndexL" $ do
    prop "Deque.elemIndexL p == elemIndex p . Deque.toList" $
      \x xs ->
        D.elemIndexL x (xs :: D.Deque Int)
          == List.elemIndex x (D.toList xs)

  describe "Deque.elemIndicesR" $ do
    prop "Deque.elemIndicesR p == reverse . Deque.elemIndicesL p" $
      \x xs ->
        D.elemIndicesR x (xs :: D.Deque Int)
          == reverse (D.elemIndicesL x xs)

  describe "Deque.elemIndexR" $ do
    prop "Deque.elemIndexR p == listToMaybe . reverse . D.elemIndicesL p" $
      \x xs ->
        D.elemIndexR x (xs :: D.Deque Int)
          == (listToMaybe . reverse . D.elemIndicesL x $ xs)

  describe "Deque.breakl" $ do
    prop
      "bimap Deque.toList Deque.toList . Deque.breakl p == break p . Deque.toList"
      $ \xs ->
        Bifunc.bimap D.toList D.toList (D.breakl even (xs :: D.Deque Int))
          == break even (D.toList xs)

  describe "Deque.breakr" $ do
    prop
      "bimap Deque.toList Deque.toList . Deque.breakr p == (\\(ys, zs) -> (reverse zs, reverse ys)) . break p . reverse . Deque.toList"
      $ \xs ->
        Bifunc.bimap D.toList D.toList (D.breakr even (xs :: D.Deque Int))
          == ( (\(ys, zs) -> (reverse zs, reverse ys))
                 . break even
                 . reverse
                 $ D.toList xs
             )

  describe "Deque.spanl" $ do
    prop
      "bimap Deque.toList Deque.toList . Deque.spanl p == span p . Deque.toList"
      $ \xs ->
        Bifunc.bimap D.toList D.toList (D.spanl even (xs :: D.Deque Int))
          == span even (D.toList xs)

  describe "Deque.spanr" $ do
    prop
      "bimap Deque.toList Deque.toList . Deque.spanr p == (\\(ys, zs) -> (reverse zs, reverse ys)) . span p . reverse . Deque.toList"
      $ \xs ->
        Bifunc.bimap D.toList D.toList (D.spanr even (xs :: D.Deque Int))
          == ( (\(ys, zs) -> (reverse zs, reverse ys))
                 . span even
                 . reverse
                 $ D.toList xs
             )

  describe "Deque.dropWhileL" $ do
    prop "Deque.dropWhileL p == snd . Deque.spanl p" $
      \xs -> D.dropWhileL even (xs :: D.Deque Int) == snd (D.spanl even xs)

  describe "Deque.takeWhileL" $ do
    prop "Deque.takeWhileL p == fst . Deque.spanl p" $
      \xs -> D.takeWhileL even (xs :: D.Deque Int) == fst (D.spanl even xs)

  describe "Deque.dropWhileR" $ do
    prop "Deque.dropWhileR p == fst . Deque.spanr p" $
      \xs -> D.dropWhileR even (xs :: D.Deque Int) == fst (D.spanr even xs)

  describe "Deque.takeWhileR" $ do
    prop "Deque.takeWhileR p == snd . Deque.spanr p" $
      \xs -> D.takeWhileR even (xs :: D.Deque Int) == snd (D.spanr even xs)

  describe "Deque.partition" $ do
    prop
      "bimap Deque.toList Deque.toList . Deque.partition p == partition p . Deque.toList"
      $ \xs ->
        Bifunc.bimap D.toList D.toList (D.partition even (xs :: D.Deque Int))
          == List.partition even (D.toList xs)

  describe "Deque.filter" $ do
    prop "Deque.toList . Deque.filter p == filter p . Deque.toList" $
      \xs ->
        D.toList (D.filter even (xs :: D.Deque Int))
          == List.filter even (D.toList xs)

  describe "Deque.zip" $ do
    prop "Deque.toList . Deque.zip xs == zip (Deque.toList xs) . Deque.toList" $
      \xs ys ->
        D.toList (D.zip xs ys :: D.Deque (Int, Int))
          == zip (D.toList xs) (D.toList ys)

  describe "Deque.zipWith" $ do
    prop
      "Deque.toList . Deque.zipWith f xs == zipWith f (Deque.toList xs) . Deque.toList"
      $ \xs ys ->
        D.toList (D.zipWith (+) xs ys :: D.Deque Int)
          == zipWith (+) (D.toList xs) (D.toList ys)

  describe "Deque.unzip" $ do
    prop
      "bimap Deque.toList Deque.toList . Deque.unzip == unzip . Deque.toList"
      $ \xs ->
        Bifunc.bimap D.toList D.toList (D.unzip (xs :: D.Deque (Int, Int)))
          == unzip (D.toList xs)
