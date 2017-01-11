-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Data.Vector.Rope.Core
    ( -- * Ropes
      GenRope(..),
      -- ** Special ropes
      Rope,
      RopeB,
      RopeP,
      RopeS,
      RopeU,

      -- * Construction
      empty,

      -- * Conversion
      fromChunks,
      fromList,
      fromVector,
      toChunks,
      toList,
      toVector,
    )
    where

import Data.FingerTree (FingerTree, Measured, ViewL(..), viewl)
import qualified Data.FingerTree as Ft
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Vector as Vb
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Primitive as Vp
import Data.Vector.Rope.Measure
import qualified Data.Vector.Storable as Vs
import qualified Data.Vector.Unboxed as Vu
import Prelude hiding (length)


-- | Ropes are vectors using a chunked encoding based on finger-trees to
-- make appends and arbitrary updates efficient

newtype GenRope l v a =
    GenRope {
      fromGenRope :: FingerTree l (v a)
    }

instance (Eq a, Measured l (v a), Vector v a) => Eq (GenRope l v a) where
    xss == yss =
        getAll $
        compareBy (\xs ys -> All (V.eq xs ys))
                  (All False)
                  (All False)
                  xss
                  yss

instance (Ord a, Measured l (v a), Vector v a) => Ord (GenRope l v a) where
    compare = compareBy V.cmp LT GT

instance (Show a, Vector v a) => Show (GenRope l v a) where
    showsPrec d (GenRope xs) =
        showParen (d > 10) $
            showString "fromList " .
            (showList . concatMap V.toList . F.toList) xs


-- | Ropes that only maintain length

type Rope = GenRope Length


-- | Boxed ropes that only maintain length

type RopeB = Rope Vb.Vector


-- | Primitive ropes that only maintain length

type RopeP = Rope Vp.Vector


-- | Storable ropes that only maintain length

type RopeS = Rope Vs.Vector


-- | Unboxed ropes that only maintain length

type RopeU = Rope Vu.Vector


-- | Compare the given two ropes using the given monoid

compareBy
    :: (Measured l (v a), Monoid o, Vector v a)
    => (v a -> v a -> o)  -- ^ Comparison function
    -> o                  -- ^ If less than
    -> o                  -- ^ If greater than
    -> GenRope l v a
    -> GenRope l v a
    -> o
compareBy cmp lt gt (GenRope xss0) (GenRope yss0) =
    go (viewl xss0) (viewl yss0)

    where
    go EmptyL      EmptyL      = mempty
    go xv@EmptyL   (ys :< yss) = if V.null ys then go xv (viewl yss) else lt
    go (xs :< xss) yv@EmptyL   = if V.null xs then go (viewl xss) yv else gt
    go (xs :< xss) (ys :< yss) =
        case compare xlen ylen of
          EQ -> cmp xs ys <> go (viewl xss) (viewl yss)
          GT -> cmp xs1 ys <> go (xs2 :< xss) (viewl yss)
          LT -> cmp xs ys1 <> go (viewl xss) (ys2 :< yss)

        where
        (xs1, xs2) = V.splitAt ylen xs
        (ys1, ys2) = V.splitAt xlen ys

        xlen = V.length xs
        ylen = V.length ys


-- | Empty rope

empty :: (Measured l (v a)) => GenRope l v a
empty = GenRope Ft.empty


-- | Construct a rope from the given list of chunks

fromChunks :: (Foldable f, Measured l (v a)) => f (v a) -> GenRope l v a
fromChunks = GenRope . Ft.fromList . F.toList


-- | Construct a rope from a list

fromList :: (Foldable f, Measured l (v a), Vector v a) => f a -> GenRope l v a
fromList = fromVector . V.fromList . F.toList


-- | Construct a rope from a single chunk

fromVector :: (Measured l (v a)) => v a -> GenRope l v a
fromVector = GenRope . Ft.singleton


-- | Extract the list of chunks from the given rope

toChunks :: GenRope l v a -> [v a]
toChunks = F.toList . fromGenRope


-- | Convert the given rope into a list

toList :: (Vector v a) => GenRope l v a -> [a]
toList = concatMap V.toList . fromGenRope


-- | Convert the given rope into a vector

toVector :: (Vector v a) => GenRope l v a -> v a
toVector = V.concat . F.toList . fromGenRope
