-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Vector.Rope.Measure
    ( -- * Measures
      HasLength(..),
      Length(..)
    )
    where

import Data.FingerTree (Measured(..))
import Data.Primitive.Types (Prim)
import qualified Data.Vector as Vb
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Primitive as Vp
import qualified Data.Vector.Storable as Vs
import qualified Data.Vector.Unboxed as Vu
import Foreign.Storable (Storable)


-- | Class for finger tree measures that include length information

class (Monoid l) => HasLength l where
    -- | Extract length
    lengthMeasure :: l -> Int


-- | Length measure for vectors

newtype Length = Length { fromLength :: Int }
    deriving (Bounded, Enum, Eq, Integral, Num, Ord,
              Real, Show)

instance HasLength Length where
    lengthMeasure = fromLength

instance Measured Length (Vb.Vector a) where
    measure = Length . V.length

instance (Prim a) => Measured Length (Vp.Vector a) where
    measure = Length . V.length

instance (Storable a) => Measured Length (Vs.Vector a) where
    measure = Length . V.length

instance (Vu.Unbox a) => Measured Length (Vu.Vector a) where
    measure = Length . V.length

instance Monoid Length where
    mappend = (+)
    mempty = 0
