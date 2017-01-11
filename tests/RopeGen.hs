-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

{-# LANGUAGE FlexibleContexts #-}

module RopeGen
    ( -- * Rope generators
      EqRopes(..),
      RopeGen(..)
    )
    where

import Data.FingerTree (Measured)
import Data.Foldable
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Rope.Core as R
import Test.QuickCheck


-- | Generates pairs of ropes that are equivalent, but potentially
-- differently chunked

data EqRopes = EqRopes (R.RopeU Int) (R.RopeU Int)
    deriving (Eq, Ord)

instance Arbitrary EqRopes where
    arbitrary = do
        xs <- V.fromList <$> arbitrary
        EqRopes <$> (R.fromChunks <$> chunk xs)
                <*> (R.fromChunks <$> chunk xs)

    shrink (EqRopes xs ys) =
        map (`EqRopes` ys) (shrink xs) ++
        map (EqRopes xs) (shrink ys)

instance Show EqRopes where
    showsPrec d (EqRopes (R.GenRope xs) (R.GenRope ys)) =
        showParen (d > 10) $
            showString "EqRopes " .
            showList (map V.toList (toList xs)) .
            showChar ' ' .
            showList (map V.toList (toList ys))


-- | Generates ropes

newtype RopeGen = R { fromRopeGen :: R.RopeU Int }
    deriving (Eq, Ord)

instance Arbitrary RopeGen where
    arbitrary = R <$> arbitrary
    shrink = map R . shrink . fromRopeGen

instance Show RopeGen where
    showsPrec d = showsPrec d . fromRopeGen


-- | Orphan instance to generate ropes

instance (Arbitrary a, Measured l (v a), Vector v a) => Arbitrary (R.GenRope l v a) where
    arbitrary =
        V.fromList <$> arbitrary >>=
        fmap R.fromChunks . chunk

    shrink =
        fmap R.fromChunks .
        shrinkList (map V.fromList . shrink . V.toList) .
        R.toChunks


-- | Convert a vector into chunks of random sizes (potentially including
-- empty chunks)
--
-- > V.concat <$> chunk xs = pure xs

chunk :: (Vector v a) => v a -> Gen [v a]
chunk xs0 = go xs0
    where
    maxN = V.length xs0

    go xs' | V.null xs' = do
        stop <- arbitrary
        if stop then pure [] else fmap (V.empty :) (go xs')
    go xs' = do
        n <- choose (0, maxN)
        let (ys, xs) = V.splitAt n xs'
        fmap (ys :) (go xs)
