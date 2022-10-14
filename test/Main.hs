module Main where

import Test.Tasty
import Test.Tasty.HUnit

-- More examples available at: https://github.com/UnkindPartition/tasty
-- Run these via either `cabal test`
-- or `stack test`

import qualified TestSuite1
import qualified TestSuite2
import qualified TestSuite3
import qualified TestSuite4
import qualified TestSuite5
import qualified TestSuite6
import qualified TestSuite7

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Fprog Tests"
    [ TestSuite1.spec
    , TestSuite2.spec
    , TestSuite3.spec
    , TestSuite4.spec
    , TestSuite5.spec
    , TestSuite6.spec
    , TestSuite7.spec
    ]
