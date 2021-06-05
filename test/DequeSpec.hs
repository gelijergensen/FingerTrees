module DequeSpec where

import Control.Exception (evaluate)
import qualified Data.Bifunctor as Bifunc
import Data.Maybe (isNothing)
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
    prop "Deque.null Deque.singleton == False" $
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
