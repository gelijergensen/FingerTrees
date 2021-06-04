{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- An implementation of ordered sequences -}
module OrdSeq
  ( OrdSeq (Empty),
    empty,
    singleton,
    null,
    size,
    toList,
    fromList,
    -- fromAscList,
    -- fromDescList,
    -- fromDistinctAscList,
    -- fromDistinctDescList,
    insert,
    delete,
    member,
    -- map,
    -- mapMonotonic,
    -- union,
    -- intersection,
    -- difference,
    -- areDisjoint,
    -- isSubsetOf,
    -- isSupsetOf,
    -- support,
    -- smallestElem,
    -- kthSmallestElem,
    -- kthSmallestUniqueElem,
    -- largestElem,
    -- kthLargestElem,
    -- kthLargestUniqueElem,
    fromFoldable,
    -- fromAscFoldable,
    -- fromDescFoldable,
    -- fromDistinctAscFoldable,
    -- fromDistinctDescFoldable,
  )
where

import qualified CommonTypes as Common
import qualified Data.Bifunctor as Bifunc
import Data.Function (on)
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

newtype OrdSeq a
  = OrdSeq (Base.FingerTree (SizeLast a) (Elem a))

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

instance Ord a => Base.Measured (Elem a) (SizeLast a) where
  measure x =
    SizeLast
      { getSize = Common.Size 1,
        getLast = Common.Last $ unElem x
      }

instance Foldable OrdSeq where
  foldr f z (OrdSeq xs) = foldr f' z xs
    where
      f' a b = f (unElem a) b
  foldl f z (OrdSeq xs) = foldl f' z xs
    where
      f' a b = f a (unElem b)

instance (Show a) => Show (OrdSeq a) where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

empty :: OrdSeq a
empty = OrdSeq Base.Empty

singleton :: Ord a => a -> OrdSeq a
singleton = OrdSeq . Base.singleton . Elem

pattern Empty :: OrdSeq a
pattern Empty = OrdSeq Base.Empty

{- O(1) -}
null :: OrdSeq a -> Bool
null Empty = True
null _ = False

{- O(1) -}
size :: Ord a => OrdSeq a -> Integer
size (OrdSeq xs) = size' xs

{- O(n) -}
toList :: OrdSeq a -> [a]
toList = foldr (:) []

{- See fromFoldable -}
fromList :: Ord a => [a] -> OrdSeq a
fromList = fromFoldable

{- O(log(i)), where i <= n/2 is distance from
   insert point to nearest end -}
insert :: (Ord a) => a -> OrdSeq a -> OrdSeq a
insert a (OrdSeq xs) = OrdSeq $ Base.modify (_insert a) ((Common.Last a <=) . getLast) xs
  where
    _insert a Nothing = [Elem a]
    _insert a (Just x) = [Elem a, x]

{- O(log(i)), where i <= n/2 is distance from
   delete point to nearest end -}
delete :: (Ord a) => a -> OrdSeq a -> OrdSeq a
delete a (OrdSeq xs) = OrdSeq $ Base.modify (_delete a) ((Common.Last a <=) . getLast) xs
  where
    _delete a Nothing = []
    _delete a (Just x) = [x | a /= unElem x]

{- O(log(i)), where i <= n/2 is distance from
   member location to nearest end -}
member :: (Ord a) => a -> OrdSeq a -> Bool
member a (OrdSeq xs) =
  case Base.lookup ((Common.Last a <=) . getLast) xs of
    Nothing -> False
    Just (Elem x) -> a == x

-- Generalized functions
{- O(nlog(n)) -}
fromFoldable :: (Foldable f, Ord a) => f a -> OrdSeq a
fromFoldable = foldr insert empty

-- Helper functions
size' :: forall a. Ord a => Base.FingerTree (SizeLast a) (Elem a) -> Integer
size' xs =
  let meas = Base.measure xs :: SizeLast a
   in Common.unSize . getSize $ meas
