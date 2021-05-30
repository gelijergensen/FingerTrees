{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- An implementation of ordered multi sets -}
module MultiSet where

import Data.Function (on)
import qualified FingerTree as Base
import Prelude hiding (null)

newtype MultiSet a
  = MultiSet (Base.FingerTree (MultiSizeMax a) (MultiElem a))

data MultiElem a = MultiElem
  { getElem :: a,
    multiplicity :: Integer
  }
  deriving (Eq, Show)

data MultiSizeMax a = MultiSizeMax
  { cardinality :: Size, -- sum of all multiplicities
    supportSize :: Size, -- number of unique elements
    getMax :: Max a
  }
  deriving (Eq, Show)

data Max a
  = NegInfinity
  | Max
      { unMax :: a
      }
  deriving (Eq, Ord, Show)

newtype Size = Size
  { unSize :: Integer
  }
  deriving (Eq, Ord, Show)

instance Semigroup (MultiSizeMax a) where
  x <> y =
    MultiSizeMax
      { cardinality = cardinality x <> cardinality y,
        supportSize = supportSize x <> supportSize y,
        getMax = getMax x <> getMax y
      }

instance Monoid (MultiSizeMax a) where
  mempty =
    MultiSizeMax
      { cardinality = mempty,
        supportSize = mempty,
        getMax = mempty
      }

instance Semigroup Size where
  Size x <> Size y = Size (x + y)

instance Monoid Size where
  mempty = Size 0

instance Semigroup (Max a) where
  x <> NegInfinity = x
  _ <> x = x

instance Monoid (Max a) where
  mempty = NegInfinity

instance Base.Measured (MultiElem a) (MultiSizeMax a) where
  measure x =
    MultiSizeMax
      { cardinality = Size $ multiplicity x,
        supportSize = Size 1,
        getMax = Max $ getElem x
      }

instance Foldable MultiElem where
  foldr f z x = nTimes (fromInteger $ multiplicity x) (f $ getElem x) z

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
singleton = MultiSet . Base.singleton . singleElem

pattern Empty :: MultiSet a
pattern Empty = MultiSet Base.Empty

{- O(1) -}
null :: MultiSet a -> Bool
null Empty = True
null _ = False

{- O(1) -}
size :: MultiSet a -> Integer
size (MultiSet xs) = size' xs

{- O(1) -}
numUniqueElems :: MultiSet a -> Integer
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
insert a Empty = singleton a
insert a mset@(MultiSet xs) =
  case r of
    Base.Empty -> MultiSet $ l Base.:|> singleElem a
    x Base.:<| r' ->
      if a == getElem x
        then MultiSet $ l Base.>< (incrementElem x Base.:<| r')
        else MultiSet $ l Base.>< (singleElem a Base.:<| r)
  where
    (l, r) = Base.split ((Max a <=) . getMax) xs

{- O(log(i)), where i <= n/2 is distance from
   delete point to nearest end -}
deleteOnce :: (Ord a) => a -> MultiSet a -> MultiSet a
deleteOnce a Empty = Empty
deleteOnce a mset@(MultiSet xs) =
  case r of
    Base.Empty -> mset
    x Base.:<| r' ->
      if a == getElem x
        then case decrementElem x of
          Nothing -> MultiSet $ l Base.>< r'
          Just x' -> MultiSet $ l Base.>< (x' Base.:<| r')
        else mset
  where
    (l, r) = Base.split ((Max a <=) . getMax) xs

{- O(log(i)), where i <= n/2 is distance from
   delete point to nearest end -}
deleteEach :: (Ord a) => a -> MultiSet a -> MultiSet a
deleteEach a Empty = Empty
deleteEach a mset@(MultiSet xs) =
  case r of
    Base.Empty -> mset
    x Base.:<| r' ->
      if a == getElem x
        then MultiSet $ l Base.>< r'
        else mset
  where
    (l, r) = Base.split ((Max a <=) . getMax) xs

{- O(log(i)), where i <= n/2 is distance from
   element location to nearest end -}
count :: (Ord a) => a -> MultiSet a -> Integer
count a (MultiSet xs) =
  maybe 0 multiplicity (Base.lookup ((Max a <=) . getMax) xs)

-- Set theoretic functions
{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
union :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
union (MultiSet xs) (MultiSet ys) = MultiSet $ _union xs ys
  where
    _union Base.Empty bs = bs
    _union as Base.Empty = as
    _union as bs@(b Base.:<| bs') =
      case r of
        Base.Empty -> l Base.>< bs
        x Base.:<| r' ->
          if getElem x == getElem b
            then (l Base.:|> unionElems x b) Base.>< _union bs' r'
            else (l Base.:|> b) Base.>< _union bs' r
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
intersection :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
intersection (MultiSet xs) (MultiSet ys) = MultiSet $ _intersection xs ys
  where
    _intersection Base.Empty _ = Base.Empty
    _intersection _ Base.Empty = Base.Empty
    _intersection as (b Base.:<| bs') =
      case r of
        Base.Empty -> Base.Empty
        x Base.:<| r' ->
          if getElem x == getElem b
            then intersectElems x b Base.:<| _intersection bs' r'
            else _intersection bs' r'
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
difference :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
difference (MultiSet xs) (MultiSet ys) = MultiSet $ _difference xs ys
  where
    _difference Base.Empty _ = Base.Empty
    _difference as Base.Empty = as
    _difference as (b Base.:<| bs') =
      case r of
        Base.Empty -> l
        x Base.:<| r' ->
          if getElem x == getElem b
            then case differenceElems x b of
              Nothing -> l Base.>< differenceRest
              Just x' -> (l Base.:|> x') Base.>< differenceRest
            else differenceRest
          where
            differenceRest = _differenceReversed bs' r'
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as
    _differenceReversed Base.Empty bs = bs
    _differenceReversed _ Base.Empty = Base.Empty
    _differenceReversed as bs@(b Base.:<| bs') =
      case r of
        Base.Empty -> bs
        x Base.:<| r' ->
          if getElem x == getElem b
            then case differenceElems b x of
              Nothing -> differenceRest
              Just b' -> b Base.:<| differenceRest
            else b Base.:<| differenceRest
          where
            differenceRest = _difference bs' r'
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isDisjointFrom :: (Ord a) => MultiSet a -> MultiSet a -> Bool
isDisjointFrom (MultiSet xs) (MultiSet ys) = _isDisjointFrom xs ys
  where
    _isDisjointFrom Base.Empty _ = True
    _isDisjointFrom _ Base.Empty = True
    _isDisjointFrom as (b Base.:<| bs') =
      case r of
        Base.Empty -> True
        x Base.:<| r' -> getElem x /= getElem b && _isDisjointFrom bs' r
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isSubsetOf :: (Ord a) => MultiSet a -> MultiSet a -> Bool
isSubsetOf (MultiSet xs) (MultiSet ys) = _isSubsetOf xs ys
  where
    _isSubsetOf Base.Empty _ = True
    _isSubsetOf _ Base.Empty = False
    _isSubsetOf as bs@(b Base.:<| bs') =
      size' as <= size' bs && Base.null l && isSubsetRest
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as
        isSubsetRest =
          case r of
            Base.Empty -> True
            x Base.:<| r' ->
              if getElem x == getElem b
                then multiplicity x <= multiplicity b && _isSupsetOf bs' r'
                else _isSupsetOf bs' r
    _isSupsetOf _ Base.Empty = True
    _isSupsetOf Base.Empty _ = False
    _isSupsetOf as bs@(b Base.:<| bs') = size' as >= size' bs && isSupsetRest
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as
        isSupsetRest =
          case r of
            Base.Empty -> False
            (x Base.:<| r') ->
              getElem x == getElem b
                && multiplicity x >= multiplicity b
                && _isSubsetOf bs' r'

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isSupsetOf :: (Ord a) => MultiSet a -> MultiSet a -> Bool
isSupsetOf = flip isSubsetOf

-- Order statistics
{- O(1) -}
smallestElem :: MultiSet a -> Maybe a
smallestElem (MultiSet xs) =
  case xs of
    Base.Empty -> Nothing
    (a Base.:<| _) -> Just $ getElem a

{- O(log(min(k, n-k))) -}
kthSmallestElem :: Integer -> MultiSet a -> Maybe a
kthSmallestElem k (MultiSet xs)
  | k < 1 = Nothing
  | otherwise = getElem <$> Base.lookup ((Size k <=) . cardinality) xs

{- O(log(min(k, n-k))) -}
kthSmallestUniqueElem :: Integer -> MultiSet a -> Maybe a
kthSmallestUniqueElem k (MultiSet xs)
  | k < 1 = Nothing
  | otherwise = getElem <$> Base.lookup ((Size k <=) . supportSize) xs

{- O(1) -}
largestElem :: MultiSet a -> Maybe a
largestElem (MultiSet xs) =
  case xs of
    Base.Empty -> Nothing
    (_ Base.:|> a) -> Just $ getElem a

{- O(log(min(k, n-k))) -}
kthLargestElem :: Integer -> MultiSet a -> Maybe a
kthLargestElem k xs = kthSmallestElem (size xs - k + 1) xs

{- O(log(min(k, n-k))) -}
kthLargestUniqueElem :: Integer -> MultiSet a -> Maybe a
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
    _incrInsertElemLeft a Base.Empty = Base.singleton $ singleElem a
    _incrInsertElemLeft a xs@(x Base.:<| r) =
      if a == getElem x
        then incrementElem x Base.:<| r
        else singleElem a Base.:<| xs

{- O(n) -}
fromDescFoldable :: (Foldable f, Eq a) => f a -> MultiSet a
fromDescFoldable =
  MultiSet . foldr _incrInsertElemRight Base.empty
  where
    _incrInsertElemRight a Base.Empty = Base.singleton $ singleElem a
    _incrInsertElemRight a xs@(l Base.:|> x) =
      if a == getElem x
        then l Base.:|> incrementElem x
        else xs Base.:|> singleElem a

{- O(n) -}
fromDistinctAscFoldable :: Foldable f => f a -> MultiSet a
fromDistinctAscFoldable = MultiSet . foldr _insertElemLeft Base.empty
  where
    _insertElemLeft a xs = singleElem a Base.:<| xs

{- O(n) -}
fromDistinctDescFoldable :: Foldable f => f a -> MultiSet a
fromDistinctDescFoldable = MultiSet . foldr _insertElemRight Base.empty
  where
    _insertElemRight a xs = xs Base.:|> singleElem a

-- Helper functions
singleElem :: a -> MultiElem a
singleElem a = MultiElem {getElem = a, multiplicity = 1}

incrementElem :: MultiElem a -> MultiElem a
incrementElem x =
  MultiElem
    { getElem = getElem x,
      multiplicity = multiplicity x + 1
    }

decrementElem :: MultiElem a -> Maybe (MultiElem a)
decrementElem x
  | multiplicity x == 1 = Nothing
  | otherwise =
    Just $
      MultiElem
        { getElem = getElem x,
          multiplicity = multiplicity x - 1
        }

-- Assumes the two elements have the same Elem
unionElems :: MultiElem a -> MultiElem a -> MultiElem a
unionElems x y =
  MultiElem
    { getElem = getElem x,
      multiplicity = multiplicity x + multiplicity y
    }

-- Assumes the two elements have the same Elem
intersectElems :: MultiElem a -> MultiElem a -> MultiElem a
intersectElems x y =
  MultiElem
    { getElem = getElem x,
      multiplicity = min (multiplicity x) (multiplicity y)
    }

-- Assumes the two elements have the same Elem
differenceElems :: MultiElem a -> MultiElem a -> Maybe (MultiElem a)
differenceElems x y =
  if multiplicity x <= multiplicity y
    then Nothing
    else
      Just $
        MultiElem
          { getElem = getElem x,
            multiplicity = multiplicity x - multiplicity y
          }

size' :: forall a. Base.FingerTree (MultiSizeMax a) (MultiElem a) -> Integer
size' xs =
  let meas = Base.measure xs :: MultiSizeMax a
   in unSize . cardinality $ meas

supportSize' :: forall a. Base.FingerTree (MultiSizeMax a) (MultiElem a) -> Integer
supportSize' xs =
  let meas = Base.measure xs :: MultiSizeMax a
   in unSize . supportSize $ meas

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f = foldr1 (.) $ replicate n f
