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

import qualified CommonTypes as Common
import qualified Data.Bifunctor as Bifunc
import qualified FingerTree as Base
import Prelude hiding (map, null)

data SizeLast a = SizeLast
  { getSize :: Common.Size, -- sum of all multiplicities
    getLast :: Common.Last a -- largest element in the set
  }
  deriving (Eq, Show)

newtype Elem a = Elem
  { unElem :: a
  }
  deriving (Eq, Show)

newtype Set a = Set (Base.FingerTree (SizeLast a) (Elem a))

instance Semigroup (SizeLast a) where
  x <> y =
    SizeLast
      { getSize = getSize x <> getSize y,
        getLast = getLast x <> getLast y
      }

instance Monoid (SizeLast a) where
  mempty =
    SizeLast
      { getSize = mempty,
        getLast = mempty
      }

instance Functor SizeLast where
  fmap f x =
    SizeLast
      { getSize = getSize x,
        getLast = fmap f . getLast $ x
      }

instance Foldable Elem where
  foldr f z x = f (unElem x) z

instance Functor Elem where
  fmap f x =
    Elem
      { unElem = f $ unElem x
      }

instance Base.Measured (Elem a) (SizeLast a) where
  measure x =
    SizeLast
      { getSize = Common.Size 1,
        getLast = Common.Last $ unElem x
      }

instance Foldable Set where
  foldr f z (Set xs) = foldr f' z xs
    where
      f' a b = f (unElem a) b
  foldl f z (Set xs) = foldl f' z xs
    where
      f' a b = f a (unElem b)

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
size :: Set a -> Int
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
insert a (Set xs) = Set $ Base.modify (_insert a) ((Common.Last a <=) . getLast) xs
  where
    _insert a Nothing = [Elem a]
    _insert a (Just x) =
      if a == unElem x
        then [x]
        else [Elem a, x]

{- O(log(i)), where i <= n/2 is distance from
   delete point to nearest end -}
delete :: (Ord a) => a -> Set a -> Set a
delete a (Set xs) = Set $ Base.modify (_delete a) ((Common.Last a <=) . getLast) xs
  where
    _delete a Nothing = []
    _delete a (Just x) = [x | a /= unElem x]

{- O(log(i)), where i <= n/2 is distance from
   member location to nearest end -}
member :: (Ord a) => a -> Set a -> Bool
member a (Set xs) =
  case Base.lookup ((Common.Last a <=) . getLast) xs of
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
union (Set xs) (Set ys) = Set $ Common.unionWith const getLast xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set $ Common.intersectionWith const getLast xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
difference :: (Ord a) => Set a -> Set a -> Set a
difference (Set xs) (Set ys) =
  Set $ Common.differenceWith (\x y -> Nothing) getLast xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
areDisjoint :: (Ord a) => Set a -> Set a -> Bool
areDisjoint (Set xs) (Set ys) = Common.areDisjointWith getLast xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isSubsetOf (Set xs) (Set ys) = Common.isSubsetOfWith size' (==) getLast xs ys

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
isSupsetOf :: (Ord a) => Set a -> Set a -> Bool
isSupsetOf (Set xs) (Set ys) = Common.isSupsetOfWith size' (==) getLast xs ys

-- Order statistics
{- O(1) -}
smallestElem :: Set a -> Maybe a
smallestElem (Set xs) =
  case xs of
    Base.Empty -> Nothing
    (a Base.:<| _) -> Just $ unElem a

{- O(log(min(k, n-k))) -}
kthSmallestElem :: Int -> Set a -> Maybe a
kthSmallestElem k (Set xs)
  | k < 1 = Nothing
  | otherwise = unElem <$> Base.lookup ((Common.Size k <=) . getSize) xs

{- O(1) -}
largestElem :: Set a -> Maybe a
largestElem (Set xs) =
  case xs of
    Base.Empty -> Nothing
    (_ Base.:|> a) -> Just $ unElem a

{- O(log(min(k, n-k))) -}
kthLargestElem :: Int -> Set a -> Maybe a
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
size' :: forall a. Base.FingerTree (SizeLast a) (Elem a) -> Int
size' xs =
  let meas = Base.measure xs :: SizeLast a
   in Common.unSize . getSize $ meas
