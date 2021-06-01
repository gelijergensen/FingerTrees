{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- An implementation of ordered sets -}
module Set
  ( Set,
    empty,
    singleton,
    null,
    size,
    toList,
    fromList,
    fromAscList,
    fromDescList,
    fromDistinctAscList,
    fromDistinctDescList,
    insert,
    delete,
    member,
    map,
    mapMonotonic,
    union,
    intersection,
    difference,
    isDisjointFrom,
    isSubsetOf,
    isSupsetOf,
    smallestElem,
    kthSmallestElem,
    largestElem,
    kthLargestElem,
    fromFoldable,
    fromAscFoldable,
    fromDescFoldable,
    fromDistinctAscFoldable,
    fromDistinctDescFoldable,
  )
where

import qualified Data.Bifunctor as Bifunc
import Data.Function (on)
import qualified FingerTree as Base
import SetHelper
import Prelude hiding (map, null)

data SizeMax a = SizeMax
  { getSize :: Size,
    getMax :: Max a
  }
  deriving (Eq, Show)

newtype Elem a = Elem
  { getElem :: a
  }
  deriving (Eq, Show)

newtype Set a
  = Set (Base.FingerTree (SizeMax a) (Elem a))

instance Semigroup (SizeMax a) where
  x <> y =
    SizeMax
      { getSize = getSize x <> getSize y,
        getMax = getMax x <> getMax y
      }

instance Monoid (SizeMax a) where
  mempty =
    SizeMax
      { getSize = mempty,
        getMax = mempty
      }

instance Functor SizeMax where
  fmap f x =
    SizeMax
      { getSize = getSize x,
        getMax = fmap f . getMax $ x
      }

instance Foldable Elem where
  foldr f z x = f (getElem x) z

instance Functor Elem where
  fmap f x =
    Elem
      { getElem = f $ getElem x
      }

instance Base.Measured (Elem a) (SizeMax a) where
  measure x =
    SizeMax
      { getSize = Size 1,
        getMax = Max $ getElem x
      }

instance Foldable Set where
  foldr f z (Set xs) = foldr f' z xs
    where
      f' a b = f (getElem a) b
  foldl f z (Set xs) = foldl f' z xs
    where
      f' a b = f a (getElem b)

instance (Show a) => Show (Set a) where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

instance (Ord a) => Eq (Set a) where
  xs == ys = xs `isSubsetOf` ys && ys `isSubsetOf` xs

empty :: Set a
empty = Set Base.Empty

singleton :: a -> Set a
singleton = Set . Base.singleton . Elem

pattern Empty :: Set a
pattern Empty = Set Base.Empty

{- O(1) -}
null :: Set a -> Bool
null Empty = True
null _ = False

{- O(1) -}
size :: Set a -> Integer
size (Set xs) = size' xs

{- O(n) -}
toList :: Set a -> [a]
toList = foldr (:) []

{- See fromFoldable -}
fromList :: Ord a => [a] -> Set a
fromList = fromFoldable

{- See fromAscFoldable -}
fromAscList :: Eq a => [a] -> Set a
fromAscList = fromAscFoldable

{- See fromDescFoldable -}
fromDescList :: Eq a => [a] -> Set a
fromDescList = fromDescFoldable

{- See fromDistinctAscFoldable -}
fromDistinctAscList :: [a] -> Set a
fromDistinctAscList = fromDistinctAscFoldable

{- See fromDistinctDescFoldable -}
fromDistinctDescList :: [a] -> Set a
fromDistinctDescList = fromDistinctDescFoldable

{- O(log(i)), where i <= n/2 is distance from
   insert point to nearest end -}
insert :: (Ord a) => a -> Set a -> Set a
insert a Empty = singleton a
insert a set@(Set xs) =
  case r of
    Base.Empty -> Set $ l Base.:|> Elem a
    x Base.:<| _ ->
      if a == getElem x
        then set
        else Set $ l Base.>< (Elem a Base.:<| r)
  where
    (l, r) = Base.split ((Max a <=) . getMax) xs

{- O(log(i)), where i <= n/2 is distance from
   delete point to nearest end -}
delete :: (Ord a) => a -> Set a -> Set a
delete a Empty = Empty
delete a set@(Set xs) =
  case r of
    Base.Empty -> set
    x Base.:<| r' ->
      if a == getElem x
        then Set $ l Base.>< r'
        else set
  where
    (l, r) = Base.split ((Max a <=) . getMax) xs

{- O(log(i)), where i <= n/2 is distance from
   member location to nearest end -}
member :: (Ord a) => a -> Set a -> Bool
member a (Set xs) =
  case Base.lookup ((Max a <=) . getMax) xs of
    Nothing -> False
    Just (Elem x) -> a == x

{- O(nlog(n)) -}
map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
map f = fromList . fmap f . toList

{- O(n). Does not check for monotonicity (that x < y => f x < f y) -}
mapMonotonic :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapMonotonic f (Set xs) = Set $ Bifunc.bimap (fmap f) (fmap f) xs

-- Set theoretic functions
{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
union :: (Ord a) => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set $ _union xs ys
  where
    _union Base.Empty bs = bs
    _union as Base.Empty = as
    _union as bs@(b Base.:<| bs') =
      case r of
        Base.Empty -> l Base.>< bs
        x Base.:<| r' ->
          if x == b
            then (l Base.:|> b) Base.>< _union bs' r'
            else (l Base.:|> b) Base.>< _union bs' r
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set $ _intersection xs ys
  where
    _intersection Base.Empty _ = Base.Empty
    _intersection _ Base.Empty = Base.Empty
    _intersection as (b Base.:<| bs') =
      case r of
        Base.Empty -> Base.Empty
        x Base.:<| r' ->
          if x == b
            then b Base.:<| _intersection bs' r'
            else _intersection bs' r'
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
difference :: (Ord a) => Set a -> Set a -> Set a
difference (Set xs) (Set ys) = Set $ _difference xs ys
  where
    _difference Base.Empty _ = Base.Empty
    _difference as Base.Empty = as
    _difference as (b Base.:<| bs') =
      case r of
        Base.Empty -> l
        x Base.:<| r' ->
          if x == b
            then l Base.>< differenceRest
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
          if x == b
            then differenceRest
            else b Base.:<| differenceRest
          where
            differenceRest = _difference bs' r'
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isDisjointFrom :: (Ord a) => Set a -> Set a -> Bool
isDisjointFrom (Set xs) (Set ys) = _isDisjointFrom xs ys
  where
    _isDisjointFrom Base.Empty _ = True
    _isDisjointFrom _ Base.Empty = True
    _isDisjointFrom as (b Base.:<| bs') =
      case r of
        Base.Empty -> True
        x Base.:<| r' -> x /= b && _isDisjointFrom bs' r
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isSubsetOf (Set xs) (Set ys) = _isSubsetOf xs ys
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
              if x == b
                then _isSupsetOf bs' r'
                else _isSupsetOf bs' r
    _isSupsetOf _ Base.Empty = True
    _isSupsetOf Base.Empty _ = False
    _isSupsetOf as bs@(b Base.:<| bs') = size' as >= size' bs && isSupsetRest
      where
        (l, r) = Base.split (((<=) `on` getMax) $ Base.measure b) as
        isSupsetRest =
          case r of
            Base.Empty -> False
            (x Base.:<| r') -> x == b && _isSubsetOf bs' r'

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isSupsetOf :: (Ord a) => Set a -> Set a -> Bool
isSupsetOf = flip isSubsetOf

-- Order statistics
{- O(1) -}
smallestElem :: Set a -> Maybe a
smallestElem (Set xs) =
  case xs of
    Base.Empty -> Nothing
    (a Base.:<| _) -> Just $ getElem a

{- O(log(min(k, n-k))) -}
kthSmallestElem :: Integer -> Set a -> Maybe a
kthSmallestElem k (Set xs)
  | k < 1 = Nothing
  | otherwise = getElem <$> Base.lookup ((Size k <=) . getSize) xs

{- O(1) -}
largestElem :: Set a -> Maybe a
largestElem (Set xs) =
  case xs of
    Base.Empty -> Nothing
    (_ Base.:|> a) -> Just $ getElem a

{- O(log(min(k, n-k))) -}
kthLargestElem :: Integer -> Set a -> Maybe a
kthLargestElem k xs = kthSmallestElem (size xs - k + 1) xs

-- Generalized functions
{- O(nlog(n)) -}
fromFoldable :: (Foldable f, Ord a) => f a -> Set a
fromFoldable = foldr insert empty

{- O(n) -}
fromAscFoldable :: (Foldable f, Eq a) => f a -> Set a
fromAscFoldable =
  Set . fst . foldr _maybeInsertElemLeft (Base.empty, Nothing)
  where
    _maybeInsertElemLeft a (_, Nothing) = (Base.singleton $ Elem a, Just a)
    _maybeInsertElemLeft a acc@(xs, Just lastA) =
      if a == lastA
        then acc
        else (Elem a Base.:<| xs, Just a)

{- O(n) -}
fromDescFoldable :: (Foldable f, Eq a) => f a -> Set a
fromDescFoldable =
  Set . fst . foldr _maybeInsertElemRight (Base.empty, Nothing)
  where
    _maybeInsertElemRight a (_, Nothing) = (Base.singleton $ Elem a, Just a)
    _maybeInsertElemRight a acc@(xs, Just lastA) =
      if a == lastA
        then acc
        else (xs Base.:|> Elem a, Just a)

{- O(n) -}
fromDistinctAscFoldable :: Foldable f => f a -> Set a
fromDistinctAscFoldable = Set . foldr _insertElemLeft Base.empty
  where
    _insertElemLeft a xs = Elem a Base.:<| xs

{- O(n) -}
fromDistinctDescFoldable :: Foldable f => f a -> Set a
fromDistinctDescFoldable = Set . foldr _insertElemRight Base.empty
  where
    _insertElemRight a xs = xs Base.:|> Elem a

-- Helper functions
size' :: forall a. Base.FingerTree (SizeMax a) (Elem a) -> Integer
size' xs =
  let meas = Base.measure xs :: SizeMax a
   in unSize . getSize $ meas
