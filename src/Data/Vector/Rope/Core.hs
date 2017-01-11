-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
--
-- In big-O complexity annotations variables starting with /c/ represent
-- the number of chunks and /n/ the number of individual elements.  If
-- an "optimal" complexity is annotated, it means the complexity under
-- the assumption that all chunks are non-empty.

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

      -- * Query
      (!),
      (!?),
      index,
      indexMaybe,
      length,
      null,
      uncons,
      unsnoc,

      -- * Conversion
      fromChunks,
      fromList,
      fromVector,
      toChunks,
      toList,
      toVector,
    )
    where

import Data.FingerTree (FingerTree, Measured(..), ViewL(..), ViewR(..), (<|), (|>), viewl, viewr)
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
import Prelude hiding (length, null)


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


-- | /O(min n1 n2)/ Compare the given two ropes using the given monoid

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


-- | /O(log c)/ Element at the given position, partial version of @('!?')@

(!) :: (HasLength l, Measured l (v a), Vector v a) => GenRope l v a -> Int -> a
(!) = flip index


-- | /O(log c)/ Element at the given position, 'Nothing' if out of
-- bounds

(!?) :: (HasLength l, Measured l (v a), Vector v a) => GenRope l v a -> Int -> Maybe a
(!?) = flip indexMaybe


-- | /O(1)/ Empty rope

empty :: (Measured l (v a)) => GenRope l v a
empty = GenRope Ft.empty


-- | /O(c)/ Construct a rope from the given list of chunks

fromChunks :: (Foldable f, Measured l (v a)) => f (v a) -> GenRope l v a
fromChunks = GenRope . Ft.fromList . F.toList


-- | /O(n)/ Construct a rope from a list

fromList :: (Foldable f, Measured l (v a), Vector v a) => f a -> GenRope l v a
fromList = fromVector . V.fromList . F.toList


-- | /O(1)/ Construct a rope from a single chunk

fromVector :: (Measured l (v a)) => v a -> GenRope l v a
fromVector = GenRope . Ft.singleton


-- | /O(log c)/ Element at the given position, partial version of 'indexMaybe'

index :: (HasLength l, Measured l (v a), Vector v a) => Int -> GenRope l v a -> a
index n =
    maybe (error "index: Out of bounds") id .
    indexMaybe n


-- | /O(log c)/ Element at the given position, 'Nothing' if out of
-- bounds

indexMaybe :: (HasLength l, Measured l (v a), Vector v a) => Int -> GenRope l v a -> Maybe a
indexMaybe n (GenRope xss) =
    let (xss1, xss2) = Ft.split ((> n) . lengthMeasure) xss
    in case viewl xss2 of
         EmptyL  -> Nothing
         xs :< _ -> xs V.!? (n - lengthMeasure (measure xss1))


-- | /O(1)/ Length of the given rope

length :: (HasLength l, Measured l (v a)) => GenRope l v a -> Int
length = lengthMeasure . measure . fromGenRope


-- | /O(1)/ Whether the given rope is empty

null :: (HasLength l, Measured l (v a)) => GenRope l v a -> Bool
null = (0 ==) . length


-- | /(O(c) full, O(1) per head)/ Extract the list of chunks from the
-- given rope

toChunks :: GenRope l v a -> [v a]
toChunks = F.toList . fromGenRope


-- | /(O(n) full, O(1) per head)/ Convert the given rope into a list

toList :: (Vector v a) => GenRope l v a -> [a]
toList = concatMap V.toList . fromGenRope


-- | /O(n)/ Convert the given rope into a vector

toVector :: (Vector v a) => GenRope l v a -> v a
toVector = V.concat . F.toList . fromGenRope


-- | /O(1)/ First element and the remainder of the rope

uncons :: (Measured l (v a), Vector v a) => GenRope l v a -> Maybe (a, GenRope l v a)
uncons = go . viewl . fromGenRope
    where
    go EmptyL      = Nothing
    go (xs :< xss)
        | V.null xs = go (viewl xss)
        | otherwise = Just (V.head xs, GenRope (V.tail xs <| xss))


-- | /O(1)/ First element and the remainder of the rope

unsnoc :: (Measured l (v a), Vector v a) => GenRope l v a -> Maybe (GenRope l v a, a)
unsnoc = go . viewr . fromGenRope
    where
    go EmptyR      = Nothing
    go (xss :> xs)
        | V.null xs = go (viewr xss)
        | otherwise = Just (GenRope (xss |> V.init xs), V.last xs)
