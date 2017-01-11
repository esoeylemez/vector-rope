-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Props.Query
    ( -- * Query properties
      queryProps
    )
    where

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Rope.Core as R
import RopeGen
import Test.Tasty
import Test.Tasty.QuickCheck


queryProps =
    testGroup "query" $
        testProperty "length" (matchVec1 R.length V.length) :
        []
