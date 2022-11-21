{-

| How can this test-suite be executed?
| 
| You can execute the test-suite on g0 via:

cabal repl -b base -b tasty -b tasty-hunit
:l Assignment0_TestSuite1.hs
main

| It works the same on you local machine.
| might require
cabal repl -b base -b tasty -b tasty-hunit all
| depending on cabal
|
| To execute the test with stack *locally* execute:

stack ghci --package base --package tasty --package tasty-hunit
:l Assignment0_TestSuite1.hs
main

| Please note, your submission must be located right next to the
| test-suite on your filesystem.

-}

module TestSuite7 where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

spec :: TestTree
spec = testGroup "Angabe7" [ properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [ qcProps]


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]
