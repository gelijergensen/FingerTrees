{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IntervalTree
  ( IntervalTree (Empty),
    Interval (..),
    empty,
    singleton,
    null,
    size,
    fromList,
    fromAscList,
    fromDescList,
    fromFoldable,
    fromAscFoldable,
    fromDescFoldable,
    insert,
    delete,
    member,
    map,
    mapMonotonic,
    (><),
    interval, --todo remove this
    intervals, --todo remove this!
  )
where

import qualified CommonTypes as Common
import qualified Data.Bifunctor as Bifunc
import Data.Function (on)
import Data.Maybe (maybeToList)
import qualified FingerTree as Base
import qualified OrdSeq
import Prelude hiding (map, null)

data Interval a = Interval
  { low :: a,
    high :: a
  }
  deriving (Eq)

data SizeLastMax a = SizeLastMax
  { getSize :: Common.Size,
    getLast :: Common.Last a,
    getMax :: Common.Max a
  }
  deriving (Eq, Show)

data IntervalElem a = IntervalElem
  { lowEnd :: a,
    highEnds :: OrdSeq.OrdSeq a
  }

newtype IntervalTree a
  = IntervalTree (Base.FingerTree (SizeLastMax a) (IntervalElem a))

instance Eq a => Eq (IntervalElem a) where
  x == y = lowEnd x == lowEnd y

instance Ord a => Semigroup (SizeLastMax a) where
  x <> y =
    SizeLastMax
      { getSize = getSize x <> getSize y,
        getLast = getLast x <> getLast y,
        getMax = getMax x <> getMax y
      }

instance Ord a => Monoid (SizeLastMax a) where
  mempty =
    SizeLastMax
      { getSize = mempty,
        getLast = mempty,
        getMax = mempty
      }

instance (Show a) => Show (Interval a) where
  showsPrec p Interval {low = l, high = h} =
    showParen (p > 10) $
      showString "Interval ["
        . shows l
        . showString ","
        . shows h
        . showString "]"

instance Ord a => Base.Measured (IntervalElem a) (SizeLastMax a) where
  measure x =
    SizeLastMax
      { getSize = Common.Size . OrdSeq.size $ highEnds x,
        getLast = Common.Last $ lowEnd x,
        getMax = Common.Max . OrdSeq.last $ highEnds x
      }

instance (Show a) => Show (IntervalTree a) where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

empty :: IntervalTree a
empty = IntervalTree Base.Empty

singleton :: Ord a => Interval a -> IntervalTree a
singleton = IntervalTree . Base.singleton . intervalElem

pattern Empty :: IntervalTree a
pattern Empty = IntervalTree Base.Empty

{- O(1) -}
null :: IntervalTree a -> Bool
null Empty = True
null _ = False

{- O(1) -}
size :: Ord a => IntervalTree a -> Int
size (IntervalTree xs) = size' xs

{- O(n) -}
toList :: IntervalTree a -> [Interval a]
toList (IntervalTree xs) = concatMap _toList xs
  where
    _toList x = fmap (Interval (lowEnd x)) . OrdSeq.toList $ highEnds x

{- See fromFoldable -}
fromList :: Ord a => [Interval a] -> IntervalTree a
fromList = fromFoldable

{- See fromAscFoldable -}
fromAscList :: Ord a => [Interval a] -> IntervalTree a
fromAscList = fromAscFoldable

{- See fromDescFoldable -}
fromDescList :: Ord a => [Interval a] -> IntervalTree a
fromDescList = fromDescFoldable

-- Generalized functions
{- O(nlog(n)) -}
fromFoldable :: (Foldable f, Ord a) => f (Interval a) -> IntervalTree a
fromFoldable = foldr insert empty

{- O(n) -}
fromAscFoldable :: (Foldable f, Ord a) => f (Interval a) -> IntervalTree a
fromAscFoldable = IntervalTree . foldr _insertElemLeft Base.empty
  where
    _insertElemLeft ::
      forall a.
      Ord a =>
      Interval a ->
      Base.FingerTree (SizeLastMax a) (IntervalElem a) ->
      Base.FingerTree (SizeLastMax a) (IntervalElem a)
    _insertElemLeft a Base.Empty = Base.singleton $ intervalElem a
    _insertElemLeft a xs@(x Base.:<| xs') =
      if low a == lowEnd x
        then insertHighElem a x Base.:<| xs'
        else intervalElem a Base.:<| xs

{- O(n) -}
fromDescFoldable :: (Foldable f, Ord a) => f (Interval a) -> IntervalTree a
fromDescFoldable = IntervalTree . foldr _insertElemRight Base.empty
  where
    _insertElemRight ::
      forall a.
      Ord a =>
      Interval a ->
      Base.FingerTree (SizeLastMax a) (IntervalElem a) ->
      Base.FingerTree (SizeLastMax a) (IntervalElem a)
    _insertElemRight a Base.Empty = Base.singleton $ intervalElem a
    _insertElemRight a xs@(xs' Base.:|> x) =
      if low a == lowEnd x
        then xs' Base.:|> insertHighElem a x
        else xs Base.:|> intervalElem a

{- O(log(i)), where i <= n/2 is distance from
   insert point to nearest end -}
insert :: (Ord a) => Interval a -> IntervalTree a -> IntervalTree a
insert a (IntervalTree xs) =
  IntervalTree $ Base.modify (_insert a) ((Common.Last (low a) <=) . getLast) xs
  where
    _insert a Nothing = [intervalElem a]
    _insert a (Just x) =
      if low a == lowEnd x
        then [insertHighElem a x]
        else [intervalElem a, x]

{- O(log(i)), where i <= n/2 is distance from
   delete point to nearest end -}
delete :: (Ord a) => Interval a -> IntervalTree a -> IntervalTree a
delete a (IntervalTree xs) =
  IntervalTree $ Base.modify (_delete a) ((Common.Last (low a) <=) . getLast) xs
  where
    _delete a Nothing = []
    _delete a (Just x) = maybeToList $ deleteHighElem a x

{- O(log(i)), where i <= n/2 is distance from
   member location to nearest end -}
member :: (Ord a) => Interval a -> IntervalTree a -> Bool
member a (IntervalTree xs) =
  case Base.lookup ((Common.Last (low a) <=) . getLast) xs of
    Nothing -> False
    Just x -> high a `OrdSeq.member` highEnds x

{- O(nlog(n)) -}
map :: (Ord a, Ord b) => (a -> b) -> IntervalTree a -> IntervalTree b
map f = fromList . fmap (mapInterval f) . toList

{- O(n). Does not check for monotonicity (that x < y => f x < f y) -}
mapMonotonic :: (Ord a, Ord b) => (a -> b) -> IntervalTree a -> IntervalTree b
mapMonotonic f (IntervalTree xs) =
  IntervalTree $
    Bifunc.bimap (mapSizeLastMaxMonotonic f) (mapIntervalElemMonotonic f) xs

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
infixr 5 ><

(><) :: Ord a => IntervalTree a -> IntervalTree a -> IntervalTree a
(IntervalTree xs) >< (IntervalTree ys) =
  IntervalTree $ Common.unionWith mergeIntervalElems getLast xs ys

-- (IntervalTree xs) >< (IntervalTree ys) = IntervalTree $ merge xs ys
--   where
--     merge Base.Empty bs = bs
--     merge as Base.Empty = as
--     merge as bs@(b Base.:<| bs') = case r of
--       Base.Empty -> l Base.>< bs
--       a Base.:<| r' ->
--         if lowEnd a == lowEnd b
--           then (l Base.:|> mergeIntervalElems a b) Base.>< merge bs' r'
--           else (l Base.:|> b) Base.>< merge bs' r
--       where
--         (l, r) = Base.split (((<=) `on` getLast) $ Base.measure b) as

-- Helper functions
size' ::
  forall a. Ord a => Base.FingerTree (SizeLastMax a) (IntervalElem a) -> Int
size' xs =
  let meas = Base.measure xs :: SizeLastMax a
   in Common.unSize . getSize $ meas

intervalElem :: Ord a => Interval a -> IntervalElem a
intervalElem a =
  IntervalElem
    { lowEnd = low a,
      highEnds = OrdSeq.singleton (high a)
    }

insertHighElem :: Ord a => Interval a -> IntervalElem a -> IntervalElem a
insertHighElem a x =
  IntervalElem
    { lowEnd = lowEnd x,
      highEnds = OrdSeq.insert (high a) $ highEnds x
    }

deleteHighElem ::
  Ord a => Interval a -> IntervalElem a -> Maybe (IntervalElem a)
deleteHighElem a x = case OrdSeq.delete (high a) $ highEnds x of
  OrdSeq.Empty -> Nothing
  highEnds' ->
    Just $ IntervalElem {lowEnd = lowEnd x, highEnds = highEnds'}

mergeIntervalElems ::
  Ord a => IntervalElem a -> IntervalElem a -> IntervalElem a
mergeIntervalElems x y =
  IntervalElem
    { lowEnd = lowEnd y,
      highEnds = highEnds x OrdSeq.>< highEnds y
    }

mapInterval :: Ord b => (a -> b) -> Interval a -> Interval b
mapInterval f x =
  let b1 = f $ low x
      b2 = f $ high x
   in Interval (min b1 b2) (max b1 b2)

mapSizeLastMaxMonotonic :: (a -> b) -> SizeLastMax a -> SizeLastMax b
mapSizeLastMaxMonotonic f x =
  SizeLastMax
    { getSize = getSize x,
      getLast = f <$> getLast x,
      getMax = f <$> getMax x
    }

mapIntervalElemMonotonic :: (Ord a, Ord b) => (a -> b) -> IntervalElem a -> IntervalElem b
mapIntervalElemMonotonic f x =
  IntervalElem
    { lowEnd = f $ lowEnd x,
      highEnds = OrdSeq.mapMonotonic f $ highEnds x
    }

-- todo remove this!
intervals :: Ord a => [a] -> [a] -> [Interval a]
intervals = zipWith interval

-- todo remove this!
interval :: Ord a => a -> a -> Interval a
interval x y = Interval (min x y) (max x y)
