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
    fromAscList,
    fromDescList,
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
    fromFoldable,
    fromAscFoldable,
    fromDescFoldable,
    foldlWithIndex,
    foldrWithIndex,
  )
where

import qualified CommonTypes as Common
import qualified Data.Bifunctor as Bifunc
import Data.Function (on)
import Data.Maybe (fromJust)
import qualified FingerTree as Base
import Prelude hiding (drop, head, init, last, lookup, map, null, splitAt, tail, take)

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

instance Base.Measured (Elem a) (SizeLast a) where
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

singleton :: a -> OrdSeq a
singleton = OrdSeq . Base.singleton . Elem

pattern Empty :: OrdSeq a
pattern Empty = OrdSeq Base.Empty

{- O(1) -}
null :: OrdSeq a -> Bool
null Empty = True
null _ = False

{- O(1) -}
size :: Ord a => OrdSeq a -> Int
size (OrdSeq xs) = size' xs

{- O(n) -}
toList :: OrdSeq a -> [a]
toList = foldr (:) []

{- See fromFoldable -}
fromList :: Ord a => [a] -> OrdSeq a
fromList = fromFoldable

{- See fromAscFoldable -}
fromAscList :: [a] -> OrdSeq a
fromAscList = fromAscFoldable

{- See fromDescFoldable -}
fromDescList :: [a] -> OrdSeq a
fromDescList = fromDescFoldable

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

{- O(nlog(n)) -}
map :: (Ord a, Ord b) => (a -> b) -> OrdSeq a -> OrdSeq b
map f = fromList . fmap f . toList

{- O(n). Does not check for monotonicity (that x < y => f x < f y) -}
mapMonotonic :: (Ord a, Ord b) => (a -> b) -> OrdSeq a -> OrdSeq b
mapMonotonic f (OrdSeq xs) = OrdSeq $ Bifunc.bimap (fmap f) (fmap f) xs

{- Probably amortized O(m log(n/m + 1),
   where m <= n lengths of xs and ys -}
infixr 5 ><

(><) :: Ord a => OrdSeq a -> OrdSeq a -> OrdSeq a
(OrdSeq xs) >< (OrdSeq ys) = OrdSeq $ merge xs ys
  where
    merge Base.Empty bs = bs
    merge as Base.Empty = as
    merge as bs@(b Base.:<| bs') = case r of
      Base.Empty -> l Base.>< bs
      a Base.:<| r' ->
        (l Base.:|> b Base.:|> a) Base.>< merge bs' r'
      where
        (l, r) = Base.split (((<=) `on` getLast) $ Base.measure b) as

{- O(1) -}
head :: OrdSeq a -> a
head (OrdSeq (x Base.:<| _)) = unElem x

{- amortized O(1), worst case O(log(n)) -}
tail :: OrdSeq a -> OrdSeq a
tail (OrdSeq (_ Base.:<| xs)) = OrdSeq xs

{- O(1) -}
last :: OrdSeq a -> a
last (OrdSeq (_ Base.:|> x)) = unElem x

{- amortized O(1), worst case O(log(n)) -}
init :: OrdSeq a -> OrdSeq a
init (OrdSeq (xs Base.:|> _)) = OrdSeq xs

{- O(log(min(i, n-i))) -}
lookup :: Int -> OrdSeq a -> Maybe a
lookup i (OrdSeq xs)
  | i < 0 = Nothing
  | otherwise = unElem <$> Base.lookup ((Common.Size i <) . getSize) xs

{- O(log(min(i, n-i))) -}
(!?) :: OrdSeq a -> Int -> Maybe a
(!?) = flip lookup

{- O(log(min(i, n-i))) -}
index :: OrdSeq a -> Int -> a
index xs@(OrdSeq xs') i
  | i < 0 || i >= length xs =
    error $ "Index out of bounds in call to: Deque.index " ++ show i
  | otherwise = unElem . fromJust $ Base.lookup ((Common.Size i <) . getSize) xs'

{- O(log(min(i, n-i))) -}
take :: Int -> OrdSeq a -> OrdSeq a
take i = fst . splitAt i

{- O(log(min(i, n-i))) -}
drop :: Int -> OrdSeq a -> OrdSeq a
drop i = snd . splitAt i

{- O(log(min(i, n-i))) -}
splitAt :: Int -> OrdSeq a -> (OrdSeq a, OrdSeq a)
splitAt i (OrdSeq xs) = (OrdSeq l, OrdSeq r)
  where
    (l, r) = Base.split ((Common.Size i <) . getSize) xs

-- Generalized functions
{- O(nlog(n)) -}
fromFoldable :: (Foldable f, Ord a) => f a -> OrdSeq a
fromFoldable = foldr insert empty

{- O(n) -}
fromAscFoldable :: Foldable f => f a -> OrdSeq a
fromAscFoldable = OrdSeq . foldr _insertElemLeft Base.empty
  where
    _insertElemLeft a xs = Elem a Base.:<| xs

{- O(n) -}
fromDescFoldable :: Foldable f => f a -> OrdSeq a
fromDescFoldable = OrdSeq . foldr _insertElemRight Base.empty
  where
    _insertElemRight a xs = xs Base.:|> Elem a

{- O(n) -}
foldlWithIndex :: (b -> Int -> a -> b) -> b -> OrdSeq a -> b
foldlWithIndex f z xs = fst $ foldl f' (z, 0) xs
  where
    f' (b, i) a = (f b i a, i + 1)

{- O(n) -}
foldrWithIndex :: (Int -> a -> b -> b) -> b -> OrdSeq a -> b
foldrWithIndex f z xs = fst $ foldr f' (z, length xs - 1) xs
  where
    f' a (b, i) = (f i a b, i - 1)

-- Helper functions
size' :: forall a. Base.FingerTree (SizeLast a) (Elem a) -> Int
size' xs =
  let meas = Base.measure xs :: SizeLast a
   in Common.unSize . getSize $ meas
