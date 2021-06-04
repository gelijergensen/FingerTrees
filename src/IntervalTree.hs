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
    interval, --todo remove this
    intervals, --todo remove this!
  )
where

import qualified CommonTypes as Common
import Data.Maybe (maybeToList)
import qualified FingerTree as Base
import qualified OrdSeq
import Prelude hiding (null)

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
  { lowElem :: a,
    highElems :: OrdSeq.OrdSeq a
  }

newtype IntervalTree a
  = IntervalTree (Base.FingerTree (SizeLastMax a) (IntervalElem a))

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
      { getSize = Common.Size . OrdSeq.size $ highElems x,
        getLast = Common.Last $ lowElem x,
        getMax = Common.Max . OrdSeq.last $ highElems x
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
    _toList x = map (Interval (lowElem x)) . OrdSeq.toList $ highElems x

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
      if low a == lowElem x
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
      if low a == lowElem x
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
      if low a == lowElem x
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
    Just x -> high a `OrdSeq.member` highElems x

-- Helper functions
size' ::
  forall a. Ord a => Base.FingerTree (SizeLastMax a) (IntervalElem a) -> Int
size' xs =
  let meas = Base.measure xs :: SizeLastMax a
   in Common.unSize . getSize $ meas

intervalElem :: Ord a => Interval a -> IntervalElem a
intervalElem a =
  IntervalElem
    { lowElem = low a,
      highElems = OrdSeq.singleton (high a)
    }

insertHighElem :: Ord a => Interval a -> IntervalElem a -> IntervalElem a
insertHighElem a x =
  IntervalElem
    { lowElem = lowElem x,
      highElems = OrdSeq.insert (high a) $ highElems x
    }

deleteHighElem ::
  Ord a => Interval a -> IntervalElem a -> Maybe (IntervalElem a)
deleteHighElem a x = case OrdSeq.delete (high a) $ highElems x of
  OrdSeq.Empty -> Nothing
  highElems' ->
    Just $ IntervalElem {lowElem = lowElem x, highElems = highElems'}

-- todo remove this!
intervals :: Ord a => [a] -> [a] -> [Interval a]
intervals = zipWith interval

-- todo remove this!
interval :: Ord a => a -> a -> Interval a
interval x y = Interval (min x y) (max x y)
