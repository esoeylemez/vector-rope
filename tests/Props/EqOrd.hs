-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Props.EqOrd
    ( -- * Equality properties
      eqOrdProps
    )
    where

import qualified Data.Vector.Rope.Core as R
import RopeGen
import Test.Tasty
import Test.Tasty.QuickCheck


eqEq (EqRopes xs ys) =
    xs == ys && ys == xs

eqNEq (R xs) (R ys) =
    R.toVector xs /= R.toVector ys ==> xs /= ys


eqOrdProps =
    testGroup "EqOrd" $
        testProperty "==" eqEq :
        testProperty "/=" eqNEq :
        testProperty "compare" (matchVec2 compare compare) :
        []
