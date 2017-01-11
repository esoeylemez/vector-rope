-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Props.Query
    ( -- * Query properties
      queryProps
    )
    where

import Data.FingerTree (Measured)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Rope.Core as R
import RopeGen
import Test.Tasty
import Test.Tasty.QuickCheck


unconsV :: (Measured l (v a), V.Vector v a) => v a -> Maybe (a, R.GenRope l v a)
unconsV xs
    | V.null xs = Nothing
    | otherwise = Just (V.head xs, R.fromVector (V.tail xs))


unsnocV :: (Measured l (v a), V.Vector v a) => v a -> Maybe (R.GenRope l v a, a)
unsnocV xs
    | V.null xs = Nothing
    | otherwise = Just (R.fromVector (V.init xs), V.last xs)


queryProps =
    testGroup "query" $
        testProperty "indexMaybe" (\n -> matchVec1 (R.indexMaybe n) (V.!? n)) :
        testProperty "length" (matchVec1 R.length V.length) :
        testProperty "uncons" (matchVec1 R.uncons unconsV) :
        testProperty "unsnoc" (matchVec1 R.unsnoc unsnocV) :
        []
