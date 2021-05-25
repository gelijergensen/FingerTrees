{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified FingerTree as Base
import Prelude hiding (null)

newtype Set a
  = Set (Base.FingerTree (MultiSizeMax a) (MultiElem a))

data MultiElem a = MultiElem
  { getElem :: a,
    multiplicity :: Integer
  }
  deriving (Eq, Show)

data MultiSizeMax a = MultiSizeMax
  { cardinality :: Size, -- sum of all multiplicities
    supportSize :: Size, -- number of unique elements
    getMax :: Max a
  }
  deriving (Eq, Show)

data Max a
  = NegInfinity
  | Max
      { unMax :: a
      }
  deriving (Eq, Ord, Show)

newtype Size = Size
  { unSize :: Integer
  }
  deriving (Eq, Ord, Show)

instance Semigroup (MultiSizeMax a) where
  x <> y =
    MultiSizeMax
      { cardinality = cardinality x <> cardinality y,
        supportSize = supportSize x <> supportSize y,
        getMax = getMax x <> getMax y
      }

instance Monoid (MultiSizeMax a) where
  mempty =
    MultiSizeMax
      { cardinality = mempty,
        supportSize = mempty,
        getMax = mempty
      }

instance Semigroup Size where
  Size x <> Size y = Size (x + y)

instance Monoid Size where
  mempty = Size 0

instance Semigroup (Max a) where
  x <> NegInfinity = x
  _ <> x = x

instance Monoid (Max a) where
  mempty = NegInfinity

instance Base.Measured (MultiElem a) (MultiSizeMax a) where
  measure x =
    MultiSizeMax
      { cardinality = Size $ multiplicity x,
        supportSize = Size 1,
        getMax = Max $ getElem x
      }
