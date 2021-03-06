-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
--
-- In big-O complexity annotations variables starting with /c/ represent
-- the number of chunks and /n/ the number of individual elements.  If
-- an "optimal" complexity is annotated, it means the complexity under
-- the assumption that all chunks are non-empty.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

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
      concatMap,
      empty,
      singleton,

      -- * Query
      (!),
      (!?),
      head,
      index,
      indexMaybe,
      init,
      last,
      length,
      null,
      tail,
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

import Control.Monad
import Data.FingerTree (FingerTree, Measured(..), ViewL(..), ViewR(..), (<|), (|>), viewl, viewr)
import qualified Data.FingerTree as Ft
import qualified Data.Foldable as F
import Data.Monoid
import Data.String
import qualified Data.Vector as Vb
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Primitive as Vp
import Data.Vector.Rope.Measure
import qualified Data.Vector.Storable as Vs
import qualified Data.Vector.Unboxed as Vu
import qualified Prelude as P
import Prelude hiding (concatMap, head, init, last, length, null, tail)


-- | Ropes are vectors using a chunked encoding based on finger-trees to
-- make appends and arbitrary updates efficient

newtype GenRope l v a =
    GenRope {
      fromGenRope :: FingerTree l (v a)
    }

instance Applicative (GenRope Length Vb.Vector) where
    pure = singleton
    (<*>) = ap

instance (Eq a, Measured l (v a), Vector v a) => Eq (GenRope l v a) where
    xss == yss =
        getAll $
        compareBy (\xs ys -> All (V.eq xs ys))
                  (All False)
                  (All False)
                  xss
                  yss

instance Foldable (GenRope Length Vb.Vector) where
    foldMap f = foldMap f . toList

instance Functor (GenRope Length Vb.Vector) where
    fmap f = GenRope . Ft.fmap' (fmap f) . fromGenRope

instance (Measured l (v Char), Vector v Char) => IsString (GenRope l v Char) where
    fromString = fromList

instance Monad (GenRope Length Vb.Vector) where
    (>>=) = flip concatMap

instance (Measured l (v a)) => Monoid (GenRope l v a) where
    mappend (GenRope xs) (GenRope ys) = GenRope (xs <> ys)
    mempty = empty

instance (Ord a, Measured l (v a), Vector v a) => Ord (GenRope l v a) where
    compare = compareBy V.cmp LT GT

instance (Show a, Vector v a) => Show (GenRope l v a) where
    showsPrec d (GenRope xs) =
        showParen (d > 10) $
            showString "fromList " .
            (showList . P.concatMap V.toList . F.toList) xs

instance Traversable (GenRope Length Vb.Vector) where
    traverse f =
        fmap (GenRope . Ft.fromList) .
        traverse (traverse f) .
        F.toList .
        fromGenRope


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


-- | /O(n)/ Map the given function and concatenate the results

concatMap
    :: (Measured l (v b), Vector v' a)
    => (a -> GenRope l v b)
    -> GenRope l' v' a
    -> GenRope l v b
concatMap f = foldMap f . toList


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


-- | /(O(c), optimal O(1))/ First element, throws if empty

head :: (Measured l (v a), Vector v a) => GenRope l v a -> a
head =
    maybe (error "head: Empty rope") fst .
    uncons


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


-- | /(O(c), optimal O(1))/ All but last element, throws if empty

init :: (Measured l (v a), Vector v a) => GenRope l v a -> GenRope l v a
init =
    maybe (error "init: Empty rope") fst .
    unsnoc


-- | /(O(c), optimal O(1))/ Last element, throws if empty

last :: (Measured l (v a), Vector v a) => GenRope l v a -> a
last =
    maybe (error "last: Empty rope") snd .
    unsnoc


-- | /O(1)/ Length of the given rope

length :: (HasLength l, Measured l (v a)) => GenRope l v a -> Int
length = lengthMeasure . measure . fromGenRope


-- | /O(1)/ Whether the given rope is empty

null :: (HasLength l, Measured l (v a)) => GenRope l v a -> Bool
null = (0 ==) . length


-- | /O(1)/ Singleton rope

singleton :: (Measured l (v a), Vector v a) => a -> GenRope l v a
singleton = GenRope . Ft.singleton . V.singleton


-- | /(O(c), optimal O(1))/ All but first element, throws if empty

tail :: (Measured l (v a), Vector v a) => GenRope l v a -> GenRope l v a
tail =
    maybe (error "tail: Empty rope") snd .
    uncons


-- | /(O(c) full, O(1) per head)/ Extract the list of chunks from the
-- given rope

toChunks :: GenRope l v a -> [v a]
toChunks = F.toList . fromGenRope


-- | /(O(n) full, O(1) per head)/ Convert the given rope into a list

toList :: (Vector v a) => GenRope l v a -> [a]
toList = P.concatMap V.toList . fromGenRope


-- | /O(n)/ Convert the given rope into a vector

toVector :: (Vector v a) => GenRope l v a -> v a
toVector = V.concat . F.toList . fromGenRope


-- | /(O(c), optimal O(1))/ First element and the remainder of the rope

uncons :: (Measured l (v a), Vector v a) => GenRope l v a -> Maybe (a, GenRope l v a)
uncons = go . viewl . fromGenRope
    where
    go EmptyL      = Nothing
    go (xs :< xss)
        | V.null xs = go (viewl xss)
        | otherwise = Just (V.head xs, GenRope (V.tail xs <| xss))


-- | /(O(c), optimal O(1))/ First element and the remainder of the rope

unsnoc :: (Measured l (v a), Vector v a) => GenRope l v a -> Maybe (GenRope l v a, a)
unsnoc = go . viewr . fromGenRope
    where
    go EmptyR      = Nothing
    go (xss :> xs)
        | V.null xs = go (viewr xss)
        | otherwise = Just (GenRope (xss |> V.init xs), V.last xs)
