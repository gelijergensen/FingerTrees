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
  )
where

import Control.Applicative (liftA2)
import qualified Data.Bifunctor as Bifunc
import Data.Maybe (fromJust)
import Data.Traversable (mapAccumL, mapAccumR)
import qualified FingerTree as Base
import Prelude hiding
  ( drop,
    head,
    init,
    last,
    length,
    lookup,
    null,
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

adjustAt :: Int -> (a -> a) -> Deque a -> Deque a
adjustAt i f (Deque xs) = Deque $ Base.modify f' (Size i <) xs
  where
    f' Nothing = []
    f' (Just x) = [fmap f x]

insertAt :: Int -> a -> Deque a -> Deque a
insertAt i a (Deque xs) = Deque $ Base.modify f (Size i <) xs
  where
    f Nothing = [Elem a]
    f (Just x) = [Elem a, x]

deleteAt :: Int -> Deque a -> Deque a
deleteAt i (Deque xs) = Deque $ Base.modify (const []) (Size i <) xs

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

foldMapWithIndex :: Monoid m => (Int -> a -> m) -> Deque a -> m
foldMapWithIndex f = foldMap (uncurry f) . snd . mapAccumL withIndex 0
  where
    withIndex i x = (i + 1, (i, x))

foldlWithIndex :: (b -> Int -> a -> b) -> b -> Deque a -> b
foldlWithIndex f z xs = fst $ foldl f' (z, 0) xs
  where
    f' (b, i) a = (f b i a, i + 1)

foldrWithIndex :: (Int -> a -> b -> b) -> b -> Deque a -> b
foldrWithIndex f z xs = fst $ foldr f' (z, 0) xs
  where
    f' a (b, i) = (f i a b, i + 1)

mapWithIndex :: (Int -> a -> b) -> Deque a -> Deque b
mapWithIndex f = snd . mapAccumL f' 0
  where
    f' i x = (i + 1, f i x)

traverseWithIndex :: Applicative f => (Int -> a -> f b) -> Deque a -> f (Deque b)
traverseWithIndex f = traverse (uncurry f) . snd . mapAccumL withIndex 0
  where
    withIndex i x = (i + 1, (i, x))

scanl :: (b -> a -> b) -> b -> Deque a -> Deque b
scanl f z0 as = z0 :<| snd (mapAccumL (\z a -> let b = f z a in (b, b)) z0 as)

scanl1 :: (a -> a -> a) -> Deque a -> Deque a
scanl1 f xs = case viewL xs of
  NilL -> error "Empty deque encountered in call to Deque.scanl1"
  ConsL x xs' -> scanl f x xs'

scanr :: (a -> b -> b) -> b -> Deque a -> Deque b
scanr f z0 as = snd (mapAccumR (\z a -> let b = f a z in (b, b)) z0 as) :|> z0

scanr1 :: (a -> a -> a) -> Deque a -> Deque a
scanr1 f xs = case viewR xs of
  NilR -> error "Empty deque encountered in call to Deque.scanr1"
  ConsR xs' x -> scanr f x xs'

-- {- O(n) -}
-- findIndicesL :: (a -> Bool) -> Deque a -> [Int]
-- findIndicesL p xs = viewL xs
--   where
