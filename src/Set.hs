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
    areDisjoint,
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
insert a (Set xs) = Set $ Base.modify (_insert a) ((Max a <=) . getMax) xs
  where
    _insert a Nothing = [Elem a]
    _insert a (Just x) =
      if a == getElem x
        then [x]
        else [Elem a, x]

{- O(log(i)), where i <= n/2 is distance from
   delete point to nearest end -}
delete :: (Ord a) => a -> Set a -> Set a
delete a (Set xs) = Set $ Base.modify (_delete a) ((Max a <=) . getMax) xs
  where
    _delete a Nothing = []
    _delete a (Just x) = [x | a /= getElem x]

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
union (Set xs) (Set ys) = Set $ unionWith const getMax xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set $ intersectionWith const getMax xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
difference :: (Ord a) => Set a -> Set a -> Set a
difference (Set xs) (Set ys) =
  Set $ differenceWith (\x y -> Nothing) getMax xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
areDisjoint :: (Ord a) => Set a -> Set a -> Bool
areDisjoint (Set xs) (Set ys) = areDisjointWith getMax xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isSubsetOf (Set xs) (Set ys) = isSubsetOfWith size' (==) getMax xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isSupsetOf :: (Ord a) => Set a -> Set a -> Bool
isSupsetOf (Set xs) (Set ys) = isSupsetOfWith size' (==) getMax xs ys

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
