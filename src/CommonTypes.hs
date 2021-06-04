{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CommonTypes where

import Data.Function (on)
import qualified FingerTree as Base

data Last a
  = NoLast
  | Last a
  deriving (Eq, Ord, Show)

data Max a
  = NegInfinity
  | Max a
  deriving (Eq, Ord, Show)

newtype Size = Size
  { unSize :: Int
  }
  deriving (Eq, Ord, Show)

instance Semigroup (Last a) where
  x <> NoLast = x
  _ <> x = x

instance Monoid (Last a) where
  mempty = NoLast

instance Functor Last where
  fmap _ NoLast = NoLast
  fmap f (Last x) = Last $ f x

instance Ord a => Semigroup (Max a) where
  x <> NegInfinity = x
  NegInfinity <> x = x
  Max x <> Max y = Max $ x `max` y

instance Ord a => Monoid (Max a) where
  mempty = NegInfinity

instance Functor Max where
  fmap _ NegInfinity = NegInfinity
  fmap f (Max x) = Max $ f x

instance Semigroup Size where
  Size x <> Size y = Size (x + y)

instance Monoid Size where
  mempty = Size 0

unionWith ::
  (Base.Measured a v, Eq a, Ord w) =>
  (a -> a -> a) ->
  (v -> w) ->
  Base.FingerTree v a ->
  Base.FingerTree v a ->
  Base.FingerTree v a
unionWith _ _ Base.Empty bs = bs
unionWith _ _ as Base.Empty = as
unionWith fMerge fMeas as bs@(b Base.:<| bs') =
  case r of
    Base.Empty -> l Base.>< bs
    a Base.:<| r' ->
      if a == b
        then (l Base.:|> fMerge a b) Base.>< unionWith fMerge fMeas bs' r'
        else (l Base.:|> b) Base.>< unionWith fMerge fMeas bs' r
  where
    (l, r) = Base.split (((<=) `on` fMeas) $ Base.measure b) as

intersectionWith ::
  (Base.Measured a v, Eq a, Ord w) =>
  (a -> a -> a) ->
  (v -> w) ->
  Base.FingerTree v a ->
  Base.FingerTree v a ->
  Base.FingerTree v a
intersectionWith _ _ Base.Empty _ = Base.Empty
intersectionWith _ _ _ Base.Empty = Base.Empty
intersectionWith fMerge fMeas as bs@(b Base.:<| bs') =
  case r of
    Base.Empty -> Base.Empty
    a Base.:<| r' ->
      if a == b
        then fMerge a b Base.:<| intersectionWith fMerge fMeas bs' r'
        else intersectionWith fMerge fMeas bs' r
  where
    (l, r) = Base.split (((<=) `on` fMeas) $ Base.measure b) as

differenceWith ::
  (Base.Measured a v, Eq a, Ord w) =>
  (a -> a -> Maybe a) ->
  (v -> w) ->
  Base.FingerTree v a ->
  Base.FingerTree v a ->
  Base.FingerTree v a
differenceWith _ _ Base.Empty _ = Base.Empty
differenceWith _ _ as Base.Empty = as
differenceWith fMerge fMeas as (b Base.:<| bs') =
  case r of
    Base.Empty -> l
    a Base.:<| r' ->
      if a == b
        then case fMerge a b of
          Nothing -> l Base.>< reversedDifferenceWith fMerge fMeas bs' r'
          Just a' ->
            (l Base.:|> a') Base.>< reversedDifferenceWith fMerge fMeas bs' r'
        else l Base.>< reversedDifferenceWith fMerge fMeas bs' r
  where
    (l, r) = Base.split (((<=) `on` fMeas) $ Base.measure b) as

reversedDifferenceWith ::
  (Base.Measured a v, Eq a, Ord w) =>
  (a -> a -> Maybe a) ->
  (v -> w) ->
  Base.FingerTree v a ->
  Base.FingerTree v a ->
  Base.FingerTree v a
reversedDifferenceWith _ _ Base.Empty bs = bs
reversedDifferenceWith _ _ _ Base.Empty = Base.Empty
reversedDifferenceWith fMerge fMeas as bs@(b Base.:<| bs') =
  case r of
    Base.Empty -> bs
    a Base.:<| r' ->
      if a == b
        then case fMerge b a of -- yes, order flipped here
          Nothing -> differenceWith fMerge fMeas bs' r'
          Just b' -> b' Base.:<| differenceWith fMerge fMeas bs' r'
        else b Base.:<| differenceWith fMerge fMeas bs' r
  where
    (l, r) = Base.split (((<=) `on` fMeas) $ Base.measure b) as

areDisjointWith ::
  (Base.Measured a v, Eq a, Ord w) =>
  (v -> w) ->
  Base.FingerTree v a ->
  Base.FingerTree v a ->
  Bool
areDisjointWith _ Base.Empty _ = True
areDisjointWith _ _ Base.Empty = True
areDisjointWith fMeas as (b Base.:<| bs') =
  case r of
    Base.Empty -> True
    a Base.:<| r' -> a /= b && areDisjointWith fMeas bs' r
  where
    (l, r) = Base.split (((<=) `on` fMeas) $ Base.measure b) as

isSubsetOfWith ::
  (Base.Measured a v, Eq a, Ord w) =>
  (Base.FingerTree v a -> Int) ->
  (a -> a -> Bool) ->
  (v -> w) ->
  Base.FingerTree v a ->
  Base.FingerTree v a ->
  Bool
isSubsetOfWith _ _ _ Base.Empty _ = True
isSubsetOfWith _ _ _ _ Base.Empty = False
isSubsetOfWith fSize fLeq fMeas as bs@(b Base.:<| bs') =
  fSize as <= fSize bs && Base.null l && isSubsetRest
  where
    (l, r) = Base.split (((<=) `on` fMeas) $ Base.measure b) as
    isSubsetRest =
      case r of
        Base.Empty -> True
        a Base.:<| r' ->
          if a == b
            then fLeq a b && isSupsetOfWith fSize fLeq fMeas bs' r'
            else isSupsetOfWith fSize fLeq fMeas bs' r

isSupsetOfWith ::
  (Base.Measured a v, Eq a, Ord w) =>
  (Base.FingerTree v a -> Int) ->
  (a -> a -> Bool) ->
  (v -> w) ->
  Base.FingerTree v a ->
  Base.FingerTree v a ->
  Bool
isSupsetOfWith _ _ _ _ Base.Empty = True
isSupsetOfWith _ _ _ Base.Empty _ = False
isSupsetOfWith fSize fLeq fMeas as bs@(b Base.:<| bs') =
  fSize as >= fSize bs && isSupsetRest
  where
    (l, r) = Base.split (((<=) `on` fMeas) $ Base.measure b) as
    isSupsetRest =
      case r of
        Base.Empty -> False
        (a Base.:<| r') -> fLeq b a && isSubsetOfWith fSize fLeq fMeas bs' r'
