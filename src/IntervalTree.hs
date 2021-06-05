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
    head,
    tail,
    last,
    init,
    lookup,
    (!?),
    index,
    drop,
    take,
    splitAt,
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
import Prelude hiding
  ( drop,
    head,
    init,
    last,
    lookup,
    map,
    null,
    splitAt,
    tail,
    take,
  )

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

{- O(1) -}
head :: Ord a => IntervalTree a -> Interval a
head (IntervalTree (x Base.:<| _)) =
  Interval (lowEnd x) (OrdSeq.head $ highEnds x)

{- amortized O(1), worst case O(log(n)) -}
tail :: Ord a => IntervalTree a -> IntervalTree a
tail (IntervalTree (x Base.:<| xs)) = case OrdSeq.tail $ highEnds x of
  OrdSeq.Empty -> IntervalTree xs
  highEnds' ->
    IntervalTree $
      IntervalElem {lowEnd = lowEnd x, highEnds = highEnds'} Base.:<| xs

{- O(1) -}
last :: Ord a => IntervalTree a -> Interval a
last (IntervalTree (_ Base.:|> x)) =
  Interval (lowEnd x) (OrdSeq.last $ highEnds x)

{- amortized O(1), worst case O(log(n)) -}
init :: Ord a => IntervalTree a -> IntervalTree a
init (IntervalTree (xs Base.:|> x)) = case OrdSeq.init $ highEnds x of
  OrdSeq.Empty -> IntervalTree xs
  highEnds' ->
    IntervalTree $
      xs Base.:|> IntervalElem {lowEnd = lowEnd x, highEnds = highEnds'}

{- O(log(min(i, n-i))) -}
lookup :: Ord a => Int -> IntervalTree a -> Maybe (Interval a)
lookup i (IntervalTree xs)
  | i < 0 = Nothing
  | otherwise = case r of
    Base.Empty -> Nothing
    x Base.:<| _ ->
      Interval (lowEnd x) <$> OrdSeq.lookup (i - size' l) (highEnds x)
  where
    (l, r) = Base.split ((Common.Size i <) . getSize) xs

{- O(log(min(i, n-i))) -}
(!?) :: Ord a => IntervalTree a -> Int -> Maybe (Interval a)
(!?) = flip lookup

{- O(log(min(i, n-i))) -}
index :: Ord a => IntervalTree a -> Int -> Interval a
index xs@(IntervalTree xs') i
  | i < 0 || i >= size xs =
    error $ "Index out of bounds in call to: IntervalTree.index " ++ show i
  | otherwise =
    case r of
      x Base.:<| _ ->
        Interval (lowEnd x) $ OrdSeq.index (highEnds x) (i - size' l)
  where
    (l, r) = Base.split ((Common.Size i <) . getSize) xs'

{- O(log(min(i, n-i))) -}
take :: Ord a => Int -> IntervalTree a -> IntervalTree a
take i = fst . splitAt i

{- O(log(min(i, n-i))) -}
drop :: Ord a => Int -> IntervalTree a -> IntervalTree a
drop i = snd . splitAt i

{- O(log(min(i, n-i))) -}
splitAt :: Ord a => Int -> IntervalTree a -> (IntervalTree a, IntervalTree a)
splitAt i (IntervalTree xs) = case r of
  Base.Empty -> (IntervalTree l, Empty)
  x Base.:<| r' -> case OrdSeq.splitAt (i - size' l) $ highEnds x of
    (OrdSeq.Empty, _) -> (IntervalTree l, IntervalTree r)
    (_, OrdSeq.Empty) -> (IntervalTree $ l Base.:|> x, IntervalTree r')
    (highEndsL, highEndsR) ->
      ( IntervalTree $
          l Base.:|> IntervalElem {lowEnd = lowEnd x, highEnds = highEndsL},
        IntervalTree $
          IntervalElem {lowEnd = lowEnd x, highEnds = highEndsR} Base.:<| r'
      )
  where
    (l, r) = Base.split ((Common.Size i <) . getSize) xs

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
