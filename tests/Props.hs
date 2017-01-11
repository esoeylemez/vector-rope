-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

module Main (main) where

import Props.EqOrd
import Props.Query
import Test.Tasty


main :: IO ()
main =
    defaultMain . testGroup "GenRope" $
        eqOrdProps :
        queryProps :
        []
