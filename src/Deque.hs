{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- An implementation of deques -}
module Deque
  ( Deque (Empty, (:<|), (:|>)),
    empty,
    singleton,
    null,
    length,
    toList,
    fromList,
    fromFoldable,
    (><),
    head,
    tail,
    last,
    init,
    lookup,
    (!?),
    index,
    adjustAt,
    insertAt,
    deleteAt,
    updateAt,
    drop,
    take,
    splitAt,
    foldMapWithIndex,
    foldlWithIndex,
    foldrWithIndex,
    mapWithIndex,
    traverseWithIndex,
    scanl,
    scanl1,
    scanr,
    scanr1,
    findIndicesL,
    findIndexL,
    findIndicesR,
    findIndexR,
    elemIndicesL,
    elemIndexL,
    elemIndicesR,
    elemIndexR,
    breakl,
    breakr,
    spanl,
    spanr,
    takeWhileL,
    takeWhileR,
    dropWhileL,
    dropWhileR,
    partition,
    filter,
  )
where

import Control.Applicative (liftA2)
import qualified Data.Bifunctor as Bifunc
import Data.Maybe (fromJust, listToMaybe)
import Data.Traversable (mapAccumL, mapAccumR)
import qualified FingerTree as Base
import Prelude hiding
  ( drop,
    filter,
    head,
    init,
    last,
    length,
    lookup,
    null,
    partition,
    scanl,
    scanl1,
    scanr,
    scanr1,
    splitAt,
    tail,
    take,
  )

newtype Size = Size
  { getSize :: Int
  }
  deriving (Eq, Ord, Show)

newtype Elem a = Elem
  { getElem :: a
  }
  deriving (Eq, Show)

newtype Deque a
  = Deque (Base.FingerTree Size (Elem a))
  deriving (Eq)

data ViewL a
  = NilL
  | ConsL a (Deque a)
  deriving (Eq, Show)

data ViewR a
  = NilR
  | ConsR (Deque a) a
  deriving (Eq, Show)

instance Semigroup Size where
  Size x <> Size y = Size $ x + y

instance Monoid Size where
  mempty = Size 0

instance Foldable Elem where
  foldr f z x = f (getElem x) z

instance Functor Elem where
  fmap f x =
    Elem
      { getElem = f $ getElem x
      }

instance Base.Measured (Elem a) Size where
  measure x = Size 1

instance Foldable Deque where
  foldr f z (Deque xs) = foldr f' z xs
    where
      f' a b = f (getElem a) b
  foldl f z (Deque xs) = foldl f' z xs
    where
      f' a b = f a (getElem b)

instance Functor Deque where
  fmap f (Deque xs) = Deque $ Bifunc.second (fmap f) xs

instance Traversable Deque where
  traverse _ Empty = pure Empty
  traverse f (x :<| xs) = liftA2 (:<|) (f x) (traverse f xs)

instance (Show a) => Show (Deque a) where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

instance Functor ViewL where
  fmap _ NilL = NilL
  fmap f (ConsL x xs) = ConsL (f x) (fmap f xs)

instance Foldable ViewL where
  foldr _ z NilL = z
  foldr f z (ConsL x xs) = f x (foldr f z xs)

  foldl _ z NilL = z
  foldl f z (ConsL x xs) = foldl f (f z x) xs

instance Traversable ViewL where
  traverse _ NilL = pure NilL
  traverse f (ConsL x xs) = liftA2 ConsL (f x) (traverse f xs)

instance Functor ViewR where
  fmap _ NilR = NilR
  fmap f (ConsR xs x) = ConsR (fmap f xs) (f x)

instance Foldable ViewR where
  foldr _ z NilR = z
  foldr f z (ConsR xs x) = foldr f (f x z) xs

  foldl _ z NilR = z
  foldl f z (ConsR xs x) = f (foldl f z xs) x

instance Traversable ViewR where
  traverse _ NilR = pure NilR
  traverse f (ConsR xs x) = liftA2 ConsR (traverse f xs) (f x)

empty :: Deque a
empty = Deque Base.Empty

singleton :: a -> Deque a
singleton = Deque . Base.singleton . Elem

pattern Empty :: Deque a
pattern Empty = Deque Base.Empty

{- O(1) -}
null :: Deque a -> Bool
null Empty = True
null _ = False

{- O(1) -}
length :: Deque a -> Int
length (Deque xs) = getSize . Base.measure $ xs

{- Bidirectional pattern. See viewL and <| -}
infixr 5 :<|

pattern (:<|) ::
  a ->
  Deque a ->
  Deque a
pattern x :<| xs <-
  (viewL -> ConsL x xs)
  where
    x :<| xs = x <| xs

{- Bidirectional pattern. See viewR and |> -}
infixl 5 :|>

pattern (:|>) ::
  Deque a ->
  a ->
  Deque a
pattern xs :|> x <-
  (viewR -> ConsR xs x)
  where
    xs :|> x = xs |> x

{-# COMPLETE (:<|), Empty #-}

{-# COMPLETE (:|>), Empty #-}

{- O(n) -}
toList :: Deque a -> [a]
toList = foldr (:) []

{- See fromFoldable -}
fromList :: [a] -> Deque a
fromList = fromFoldable

{- O(nlog(n)) -}
fromFoldable :: Foldable f => f a -> Deque a
fromFoldable = Deque . foldr _insertElemLeft Base.empty
  where
    _insertElemLeft a xs = Elem a Base.:<| xs

(<|) :: a -> Deque a -> Deque a
a <| (Deque xs) = Deque $ Elem a Base.:<| xs

(|>) :: Deque a -> a -> Deque a
Deque xs |> a = Deque $ xs Base.:|> Elem a

{- O(log(m)) catenation,
   where m <= n lengths of xs and ys -}
infixr 5 ><

(><) :: Deque a -> Deque a -> Deque a
(Deque xs) >< (Deque ys) = Deque $ xs Base.>< ys

viewL :: Deque a -> ViewL a
viewL (Deque xs) = case xs of
  Base.Empty -> NilL
  x Base.:<| xs' -> ConsL (getElem x) (Deque xs')

viewR :: Deque a -> ViewR a
viewR (Deque xs) = case xs of
  Base.Empty -> NilR
  xs' Base.:|> x -> ConsR (Deque xs') (getElem x)

{- O(1) -}
head :: Deque a -> a
head (x :<| _) = x

{- amortized O(1), worst case O(log(n)) -}
tail :: Deque a -> Deque a
tail (_ :<| xs) = xs

{- O(1) -}
last :: Deque a -> a
last (_ :|> x) = x

{- amortized O(1), worst case O(log(n)) -}
init :: Deque a -> Deque a
init (xs :|> _) = xs

{- O(log(min(i, n-i))) -}
lookup :: Int -> Deque a -> Maybe a
lookup i (Deque xs)
  | i < 0 = Nothing
  | otherwise = getElem <$> Base.lookup (Size i <) xs

{- O(log(min(i, n-i))) -}
(!?) :: Deque a -> Int -> Maybe a
(!?) = flip lookup

{- O(log(min(i, n-i))) -}
index :: Deque a -> Int -> a
index xs@(Deque xs') i
  | i < 0 || i >= length xs =
    error $ "Index out of bounds in call to: Deque.index " ++ show i
  | otherwise = getElem . fromJust $ Base.lookup (Size i <) xs'

{- O(log(min(i, n-i))) -}
adjustAt :: Int -> (a -> a) -> Deque a -> Deque a
adjustAt i f (Deque xs) = Deque $ Base.modify f' (Size i <) xs
  where
    f' Nothing = []
    f' (Just x) = [fmap f x]

{- O(log(min(i, n-i))) -}
insertAt :: Int -> a -> Deque a -> Deque a
insertAt i a (Deque xs) = Deque $ Base.modify f (Size i <) xs
  where
    f Nothing = [Elem a]
    f (Just x) = [Elem a, x]

{- O(log(min(i, n-i))) -}
deleteAt :: Int -> Deque a -> Deque a
deleteAt i (Deque xs) = Deque $ Base.modify (const []) (Size i <) xs

{- O(log(min(i, n-i))) -}
updateAt :: Int -> a -> Deque a -> Deque a
updateAt i a (Deque xs) = Deque $ Base.modify f (Size i <) xs
  where
    f Nothing = []
    f _ = [Elem a]

{- O(log(min(i, n-i))) -}
take :: Int -> Deque a -> Deque a
take i = fst . splitAt i

{- O(log(min(i, n-i))) -}
drop :: Int -> Deque a -> Deque a
drop i = snd . splitAt i

{- O(log(min(i, n-i))) -}
splitAt :: Int -> Deque a -> (Deque a, Deque a)
splitAt i (Deque xs) = (Deque l, Deque r)
  where
    (l, r) = Base.split (Size i <) xs

{- O(n) -}
foldMapWithIndex :: Monoid m => (Int -> a -> m) -> Deque a -> m
foldMapWithIndex f = foldMap (uncurry f) . snd . mapAccumL withIndex 0
  where
    withIndex i x = (i + 1, (i, x))

{- O(n) -}
foldlWithIndex :: (b -> Int -> a -> b) -> b -> Deque a -> b
foldlWithIndex f z xs = fst $ foldl f' (z, 0) xs
  where
    f' (b, i) a = (f b i a, i + 1)

{- O(n) -}
foldrWithIndex :: (Int -> a -> b -> b) -> b -> Deque a -> b
foldrWithIndex f z xs = fst $ foldr f' (z, length xs - 1) xs
  where
    f' a (b, i) = (f i a b, i - 1)

{- O(n) -}
mapWithIndex :: (Int -> a -> b) -> Deque a -> Deque b
mapWithIndex f = snd . mapAccumL f' 0
  where
    f' i x = (i + 1, f i x)

{- O(n) -}
traverseWithIndex :: Applicative f => (Int -> a -> f b) -> Deque a -> f (Deque b)
traverseWithIndex f = traverse (uncurry f) . snd . mapAccumL withIndex 0
  where
    withIndex i x = (i + 1, (i, x))

{- O(n) -}
scanl :: (b -> a -> b) -> b -> Deque a -> Deque b
scanl f z0 as = z0 :<| snd (mapAccumL (\z a -> let b = f z a in (b, b)) z0 as)

{- O(n) -}
scanl1 :: (a -> a -> a) -> Deque a -> Deque a
scanl1 f xs = case viewL xs of
  NilL -> error "Empty deque encountered in call to Deque.scanl1"
  ConsL x xs' -> scanl f x xs'

{- O(n) -}
scanr :: (a -> b -> b) -> b -> Deque a -> Deque b
scanr f z0 as = snd (mapAccumR (\z a -> let b = f a z in (b, b)) z0 as) :|> z0

{- O(n) -}
scanr1 :: (a -> a -> a) -> Deque a -> Deque a
scanr1 f xs = case viewR xs of
  NilR -> error "Empty deque encountered in call to Deque.scanr1"
  ConsR xs' x -> scanr f x xs'

{- O(n) -}
findIndicesL :: (a -> Bool) -> Deque a -> [Int]
findIndicesL p = foldrWithIndex f []
  where
    f i a idxs = if p a then i : idxs else idxs

{- O(i), where i is the first matching index -}
findIndexL :: (a -> Bool) -> Deque a -> Maybe Int
findIndexL p = listToMaybe . findIndicesL p

{- O(n) -}
findIndicesR :: (a -> Bool) -> Deque a -> [Int]
findIndicesR p = foldlWithIndex f []
  where
    f idxs i a = if p a then i : idxs else idxs

{- O(i), where i is the first matching index -}
findIndexR :: (a -> Bool) -> Deque a -> Maybe Int
findIndexR p = listToMaybe . findIndicesR p

{- O(n) -}
elemIndicesL :: Eq a => a -> Deque a -> [Int]
elemIndicesL a = findIndicesL (== a)

{- O(i), where i is the first matching index -}
elemIndexL :: Eq a => a -> Deque a -> Maybe Int
elemIndexL a = listToMaybe . elemIndicesL a

{- O(n) -}
elemIndicesR :: Eq a => a -> Deque a -> [Int]
elemIndicesR a = findIndicesR (== a)

{- O(i), where i is the first matching index -}
elemIndexR :: Eq a => a -> Deque a -> Maybe Int
elemIndexR a = listToMaybe . elemIndicesR a

{- O(i), where i is the first matching index -}
breakl :: (a -> Bool) -> Deque a -> (Deque a, Deque a)
breakl p xs = case findIndexL p xs of
  Nothing -> (xs, Empty)
  Just i -> splitAt i xs

{- O(i), where i is the first matching index -}
breakr :: (a -> Bool) -> Deque a -> (Deque a, Deque a)
breakr p xs = case findIndexR p xs of
  Nothing -> (xs, Empty)
  Just i -> splitAt (i + 1) xs

{- O(i), where i is the first matching index -}
spanl :: (a -> Bool) -> Deque a -> (Deque a, Deque a)
spanl p = breakl (not . p)

{- O(i), where i is the first matching index -}
spanr :: (a -> Bool) -> Deque a -> (Deque a, Deque a)
spanr p = breakr (not . p)

{- O(i), where i is the first matching index -}
takeWhileL :: (a -> Bool) -> Deque a -> Deque a
takeWhileL p = fst . spanl p

{- O(i), where i is the first matching index -}
takeWhileR :: (a -> Bool) -> Deque a -> Deque a
takeWhileR p = fst . spanr p

{- O(i), where i is the first matching index -}
dropWhileL :: (a -> Bool) -> Deque a -> Deque a
dropWhileL p = snd . spanl p

{- O(i), where i is the first matching index -}
dropWhileR :: (a -> Bool) -> Deque a -> Deque a
dropWhileR p = snd . spanr p

{- O(n) -}
partition :: (a -> Bool) -> Deque a -> (Deque a, Deque a)
partition p = foldr f (Empty, Empty)
  where
    f a (xs, ys) = if p a then (a :<| xs, ys) else (xs, a :<| ys)

{- O(n) -}
filter :: (a -> Bool) -> Deque a -> Deque a
filter p = foldr f Empty
  where
    f a xs = if p a then a :<| xs else xs
