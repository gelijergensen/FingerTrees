{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- An implementation of ordered multi sets -}
module MultiSet
  ( MultiSet,
    empty,
    singleton,
    null,
    size,
    numUniqueElems,
    toList,
    fromList,
    fromAscList,
    fromDescList,
    fromDistinctAscList,
    fromDistinctDescList,
    insert,
    deleteOnce,
    deleteEach,
    count,
    map,
    mapMonotonic,
    union,
    intersection,
    difference,
    areDisjoint,
    isSubsetOf,
    isSupsetOf,
    support,
    smallestElem,
    kthSmallestElem,
    kthSmallestUniqueElem,
    largestElem,
    kthLargestElem,
    kthLargestUniqueElem,
    fromFoldable,
    fromAscFoldable,
    fromDescFoldable,
    fromDistinctAscFoldable,
    fromDistinctDescFoldable,
  )
where

import qualified CommonTypes as Common
import qualified Data.Bifunctor as Bifunc
import Data.Function (on)
import Data.Maybe (fromJust, maybeToList)
import qualified FingerTree as Base
import qualified Set
import Prelude hiding (map, null)

data MultiSizeLast a = MultiSizeLast
  { cardinality :: Common.Size, -- sum of all multiplicities
    supportSize :: Common.Size, -- number of unique elements
    getLast :: Common.Last a -- largest element in the multiset
  }
  deriving (Eq, Show)

data MultiElem a = MultiElem
  { unMultiElem :: a,
    multiplicity :: Int
  }
  deriving (Show)

newtype MultiSet a
  = MultiSet (Base.FingerTree (MultiSizeLast a) (MultiElem a))

instance Semigroup (MultiSizeLast a) where
  x <> y =
    MultiSizeLast
      { cardinality = cardinality x <> cardinality y,
        supportSize = supportSize x <> supportSize y,
        getLast = getLast x <> getLast y
      }

instance Monoid (MultiSizeLast a) where
  mempty =
    MultiSizeLast
      { cardinality = mempty,
        supportSize = mempty,
        getLast = mempty
      }

instance Functor MultiSizeLast where
  fmap f x =
    MultiSizeLast
      { cardinality = cardinality x,
        supportSize = supportSize x,
        getLast = fmap f . getLast $ x
      }

instance Eq a => Eq (MultiElem a) where
  x == y = unMultiElem x == unMultiElem y

instance Foldable MultiElem where
  foldr f z x = nTimes (multiplicity x) (f $ unMultiElem x) z
    where
      nTimes :: Int -> (a -> a) -> a -> a
      nTimes n f = foldr1 (.) $ replicate n f

instance Functor MultiElem where
  fmap f x =
    MultiElem
      { unMultiElem = f $ unMultiElem x,
        multiplicity = multiplicity x
      }

instance Base.Measured (MultiElem a) (MultiSizeLast a) where
  measure x =
    MultiSizeLast
      { cardinality = Common.Size $ multiplicity x,
        supportSize = Common.Size 1,
        getLast = Common.Last $ unMultiElem x
      }

instance Foldable MultiSet where
  foldr f z (MultiSet xs) = foldr f' z xs
    where
      f' = flip $ foldr f
  foldl f z (MultiSet xs) = foldl f' z xs
    where
      f' = foldl f

instance (Show a) => Show (MultiSet a) where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

instance (Ord a) => Eq (MultiSet a) where
  xs == ys = xs `isSubsetOf` ys && ys `isSubsetOf` xs

empty :: MultiSet a
empty = MultiSet Base.Empty

singleton :: a -> MultiSet a
singleton = MultiSet . Base.singleton . multiElem

pattern Empty :: MultiSet a
pattern Empty = MultiSet Base.Empty

{- O(1) -}
null :: MultiSet a -> Bool
null Empty = True
null _ = False

{- O(1) -}
size :: MultiSet a -> Int
size (MultiSet xs) = size' xs

{- O(1) -}
numUniqueElems :: MultiSet a -> Int
numUniqueElems (MultiSet xs) = supportSize' xs

{- O(n) -}
toList :: MultiSet a -> [a]
toList = foldr (:) []

{- See fromFoldable -}
fromList :: Ord a => [a] -> MultiSet a
fromList = fromFoldable

{- See fromAscFoldable -}
fromAscList :: Eq a => [a] -> MultiSet a
fromAscList = fromAscFoldable

{- See fromDescFoldable -}
fromDescList :: Eq a => [a] -> MultiSet a
fromDescList = fromDescFoldable

{- See fromDistinctAscFoldable -}
fromDistinctAscList :: [a] -> MultiSet a
fromDistinctAscList = fromDistinctAscFoldable

{- See fromDistinctDescFoldable -}
fromDistinctDescList :: [a] -> MultiSet a
fromDistinctDescList = fromDistinctDescFoldable

{- O(log(i)), where i <= n/2 is distance from
   insert point to nearest end -}
insert :: (Ord a) => a -> MultiSet a -> MultiSet a
insert a (MultiSet xs) =
  MultiSet $ Base.modify (_insert a) ((Common.Last a <=) . getLast) xs
  where
    _insert a Nothing = [multiElem a]
    _insert a (Just x) =
      if a == unMultiElem x
        then [incrementMultiElem x]
        else [multiElem a, x]

{- O(log(i)), where i <= n/2 is distance from
   delete point to nearest end -}
deleteOnce :: (Ord a) => a -> MultiSet a -> MultiSet a
deleteOnce a (MultiSet xs) = MultiSet $ Base.modify (_deleteOnce a) ((Common.Last a <=) . getLast) xs
  where
    _deleteOnce a Nothing = []
    _deleteOnce a (Just x) =
      if a == unMultiElem x
        then maybeToList $ decrementMultiElem x
        else []

{- O(log(i)), where i <= n/2 is distance from
   delete point to nearest end -}
deleteEach :: (Ord a) => a -> MultiSet a -> MultiSet a
deleteEach a (MultiSet xs) = MultiSet $ Base.modify (_deleteEach a) ((Common.Last a <=) . getLast) xs
  where
    _deleteEach a Nothing = []
    _deleteEach a (Just x) = [x | a /= unMultiElem x]

{- O(log(i)), where i <= n/2 is distance from
   element location to nearest end -}
count :: (Ord a) => a -> MultiSet a -> Int
count a (MultiSet xs) =
  maybe 0 multiplicity (Base.lookup ((Common.Last a <=) . getLast) xs)

{- O(nlog(n)) -}
map :: (Ord a, Ord b) => (a -> b) -> MultiSet a -> MultiSet b
map f = fromList . fmap f . toList

{- O(n). Does not check for monotonicity (that x < y => f x < f y) -}
mapMonotonic :: (Ord a, Ord b) => (a -> b) -> MultiSet a -> MultiSet b
mapMonotonic f (MultiSet xs) = MultiSet $ Bifunc.bimap (fmap f) (fmap f) xs

-- Set theoretic functions
{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
union :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
union (MultiSet xs) (MultiSet ys) =
  MultiSet $ Common.unionWith sumMultiElem getLast xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
intersection :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
intersection (MultiSet xs) (MultiSet ys) =
  MultiSet $ Common.intersectionWith minMultiElem getLast xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
difference :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
difference (MultiSet xs) (MultiSet ys) =
  MultiSet $ Common.differenceWith differenceMultiElem getLast xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
areDisjoint :: (Ord a) => MultiSet a -> MultiSet a -> Bool
areDisjoint (MultiSet xs) (MultiSet ys) = Common.areDisjointWith getLast xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isSubsetOf :: (Ord a) => MultiSet a -> MultiSet a -> Bool
isSubsetOf (MultiSet xs) (MultiSet ys) =
  Common.isSubsetOfWith size' ((<=) `on` multiplicity) getLast xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isSupsetOf :: (Ord a) => MultiSet a -> MultiSet a -> Bool
isSupsetOf (MultiSet xs) (MultiSet ys) =
  Common.isSupsetOfWith size' ((<=) `on` multiplicity) getLast xs ys

{- O(n) -}
support :: (Ord a) => MultiSet a -> Set.Set a
support (MultiSet xs) =
  Set.fromDistinctAscList
    . toList
    . MultiSet
    . Bifunc.second (setMultiplicity 1)
    $ xs

-- Order statistics
{- O(1) -}
smallestElem :: MultiSet a -> Maybe a
smallestElem (MultiSet xs) =
  case xs of
    Base.Empty -> Nothing
    (a Base.:<| _) -> Just $ unMultiElem a

{- O(log(min(k, n-k))) -}
kthSmallestElem :: Int -> MultiSet a -> Maybe a
kthSmallestElem k (MultiSet xs)
  | k < 1 = Nothing
  | otherwise = unMultiElem <$> Base.lookup ((Common.Size k <=) . cardinality) xs

{- O(log(min(k, n-k))) -}
kthSmallestUniqueElem :: Int -> MultiSet a -> Maybe a
kthSmallestUniqueElem k (MultiSet xs)
  | k < 1 = Nothing
  | otherwise = unMultiElem <$> Base.lookup ((Common.Size k <=) . supportSize) xs

{- O(1) -}
largestElem :: MultiSet a -> Maybe a
largestElem (MultiSet xs) =
  case xs of
    Base.Empty -> Nothing
    (_ Base.:|> a) -> Just $ unMultiElem a

{- O(log(min(k, n-k))) -}
kthLargestElem :: Int -> MultiSet a -> Maybe a
kthLargestElem k xs = kthSmallestElem (size xs - k + 1) xs

{- O(log(min(k, n-k))) -}
kthLargestUniqueElem :: Int -> MultiSet a -> Maybe a
kthLargestUniqueElem k xs = kthSmallestUniqueElem (numUniqueElems xs - k + 1) xs

-- Generalized functions
{- O(nlog(n)) -}
fromFoldable :: (Foldable f, Ord a) => f a -> MultiSet a
fromFoldable = foldr insert empty

{- O(n) -}
fromAscFoldable :: (Foldable f, Eq a) => f a -> MultiSet a
fromAscFoldable =
  MultiSet . foldr _incrInsertElemLeft Base.empty
  where
    _incrInsertElemLeft a Base.Empty = Base.singleton $ multiElem a
    _incrInsertElemLeft a xs@(x Base.:<| r) =
      if a == unMultiElem x
        then incrementMultiElem x Base.:<| r
        else multiElem a Base.:<| xs

{- O(n) -}
fromDescFoldable :: (Foldable f, Eq a) => f a -> MultiSet a
fromDescFoldable =
  MultiSet . foldr _incrInsertElemRight Base.empty
  where
    _incrInsertElemRight a Base.Empty = Base.singleton $ multiElem a
    _incrInsertElemRight a xs@(l Base.:|> x) =
      if a == unMultiElem x
        then l Base.:|> incrementMultiElem x
        else xs Base.:|> multiElem a

{- O(n) -}
fromDistinctAscFoldable :: Foldable f => f a -> MultiSet a
fromDistinctAscFoldable = MultiSet . foldr _insertElemLeft Base.empty
  where
    _insertElemLeft a xs = multiElem a Base.:<| xs

{- O(n) -}
fromDistinctDescFoldable :: Foldable f => f a -> MultiSet a
fromDistinctDescFoldable = MultiSet . foldr _insertElemRight Base.empty
  where
    _insertElemRight a xs = xs Base.:|> multiElem a

-- Helper functions
size' :: forall a. Base.FingerTree (MultiSizeLast a) (MultiElem a) -> Int
size' xs =
  let meas = Base.measure xs :: MultiSizeLast a
   in Common.unSize . cardinality $ meas

supportSize' :: forall a. Base.FingerTree (MultiSizeLast a) (MultiElem a) -> Int
supportSize' xs =
  let meas = Base.measure xs :: MultiSizeLast a
   in Common.unSize . supportSize $ meas

multiElem :: a -> MultiElem a
multiElem a =
  MultiElem
    { unMultiElem = a,
      multiplicity = 1
    }

changeMultiplicity :: (Int -> Int) -> MultiElem a -> Maybe (MultiElem a)
changeMultiplicity f x
  | newMultiplicity <= 0 = Nothing
  | otherwise =
    Just $
      MultiElem
        { unMultiElem = unMultiElem x,
          multiplicity = newMultiplicity
        }
  where
    newMultiplicity = f (multiplicity x)

setMultiplicity :: Int -> MultiElem a -> MultiElem a
setMultiplicity n = fromJust . changeMultiplicity (const n)

incrementMultiElem :: MultiElem a -> MultiElem a
incrementMultiElem = fromJust . changeMultiplicity (+ 1)

decrementMultiElem :: MultiElem a -> Maybe (MultiElem a)
decrementMultiElem = changeMultiplicity (subtract 1)

-- Assumes the two MultiElem have the same value
sumMultiElem :: MultiElem a -> MultiElem a -> MultiElem a
sumMultiElem x = fromJust . changeMultiplicity (+ multiplicity x)

-- Assumes the two MultiElem have the same value
differenceMultiElem :: MultiElem a -> MultiElem a -> Maybe (MultiElem a)
differenceMultiElem x y = changeMultiplicity (subtract . multiplicity $ y) x

-- Assumes the two MultiElem have the same value
minMultiElem :: MultiElem a -> MultiElem a -> MultiElem a
minMultiElem x = fromJust . changeMultiplicity (min . multiplicity $ x)
