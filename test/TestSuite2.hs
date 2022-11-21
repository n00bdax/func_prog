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

module TestSuite2 where

import Angabe2
  ( Baum (Blatt, Gabel),
    Baum' (Blatt', Gabel'),
    Liste (Kopf, Schluss),
    Liste' (Kopf', Schluss'),
    Lotterielos (Freilos, Niete, Treffer),
    analysiere,
    analysiere',
    analysiere'',
    analysiere''',
  )
import Data.List
import Data.Ord
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC (testProperty, (==>))

spec :: TestTree
spec = testGroup "Angabe2" [analysiereTest, analysiere'Test, analysiere''Test, analysiere'''Test]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (different length)" $
        [1, 2, 3] `compare` [1, 2] @?= GT,
      -- the following test does not hold
      testCase "List comparison (same length)" $
        [1, 2, 3] `compare` [1, 2, 2] @?= LT
    ]

qcProps :: TestTree
qcProps =
  testGroup
    "(checked by QuickCheck)"
    [ QC.testProperty "sort == sort . reverse" $
        \list -> sort (list :: [Int]) == sort (reverse list),
      QC.testProperty "Fermat's little theorem" $
        \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0,
      -- the following property does not hold
      QC.testProperty "Fermat's last theorem" $
        \x y z n ->
          (n :: Integer) >= 3 QC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
    ]

analysiereTest :: TestTree
analysiereTest =
  testGroup
    "analysiere Tests"
    [ testCase "analysiere Test 0" $
        analysiere (Kopf Niete (Kopf Treffer (Schluss Niete))) Niete Niete @?= EQ,
      testCase "analysiere Test 1" $
        analysiere (Kopf Niete (Kopf Treffer (Schluss Niete))) Niete Treffer @?= GT,
      testCase "analysiere Test 2" $
        analysiere (Kopf Niete (Kopf Treffer (Schluss Niete))) Treffer Niete @?= LT,
      testCase "analysiere Test 3" $
        analysiere (Kopf Niete (Kopf Treffer (Schluss Niete))) Treffer Treffer @?= EQ,
      testCase "analysiere Test 4" $
        analysiere (Kopf Niete (Kopf Treffer (Kopf Treffer (Schluss Niete)))) Treffer Niete @?= EQ,
      testCase "analysiere test recursive" $
        let a = Kopf Niete a in analysiere a Niete Niete @?= EQ
    ]

analysiere'Test :: TestTree
analysiere'Test =
  testGroup
    "analysiere' Tests"
    [ testCase "analysiere' Test 0" $
        analysiere' (Gabel (Blatt Treffer) Niete (Gabel (Blatt Niete) Treffer (Blatt Niete))) Niete Treffer @?= GT,
      testCase "analysiere' Test 1" $
        analysiere' (Gabel (Blatt Treffer) Niete (Gabel (Blatt Niete) Treffer (Blatt Niete))) Niete Niete @?= EQ,
      testCase "analysiere' Test 2" $
        analysiere' (Gabel (Blatt Treffer) Niete (Gabel (Blatt Niete) Treffer (Blatt Niete))) Treffer Niete @?= LT,
      testCase "analysiere' Test 3" $
        analysiere' (Gabel (Blatt Treffer) Niete (Gabel (Blatt Niete) Treffer (Blatt Niete))) Treffer Treffer @?= EQ,
      testCase "analysiere' Test 4" $
        analysiere' (Gabel (Gabel (Gabel (Blatt Treffer) Treffer (Blatt Treffer)) Treffer (Blatt Niete)) Niete (Gabel (Blatt Niete) Treffer (Blatt Niete))) Treffer Treffer @?= EQ,
      testCase "analysiere' Test 5" $
        analysiere' (Gabel (Gabel (Gabel (Blatt Treffer) Treffer (Blatt Treffer)) Treffer (Blatt Niete)) Niete (Gabel (Blatt Niete) Treffer (Blatt Niete))) Treffer Niete @?= GT,
      testCase "analysiere' Test 6" $
        analysiere' (Gabel (Gabel (Gabel (Blatt Treffer) Treffer (Blatt Treffer)) Treffer (Blatt Niete)) Niete (Gabel (Blatt Niete) Treffer (Blatt Niete))) Niete Treffer @?= LT,
      testCase "analysiere' Test 7" $
        analysiere' (Gabel (Gabel (Gabel (Blatt Treffer) Treffer (Blatt Treffer)) Treffer (Blatt Niete)) Niete (Gabel (Blatt Niete) Treffer (Blatt Niete))) Niete Niete @?= EQ,
      testCase "analysiere' Test 8" $
        analysiere' (Gabel (Gabel (Gabel (Blatt Treffer) Treffer (Blatt Freilos)) Treffer (Blatt Niete)) Niete (Gabel (Blatt Niete) Treffer (Blatt Niete))) Niete Treffer @?= EQ,
      testCase "analysiere' test recursive" $
        let a = Gabel a Niete a in analysiere' a Niete Niete @?= EQ
    ]

analysiere''Test :: TestTree
analysiere''Test =
  testGroup
    "analysiere'' Tests"
    [ testCase "analysiere'' Test 0" $
        analysiere'' (Kopf' (Gabel (Blatt Treffer) Niete (Blatt Niete)) (Kopf' (Blatt Treffer) (Schluss' (Gabel (Blatt Niete) Treffer (Blatt Niete))))) Niete Treffer @?= GT,
      testCase "analysiere'' Test 1" $
        analysiere'' (Kopf' (Gabel (Blatt Treffer) Niete (Blatt Niete)) (Kopf' (Blatt Treffer) (Schluss' (Gabel (Blatt Niete) Treffer (Blatt Niete))))) Niete Niete @?= EQ,
      testCase "analysiere'' Test 2" $
        analysiere'' (Kopf' (Gabel (Blatt Treffer) Niete (Blatt Niete)) (Kopf' (Blatt Treffer) (Schluss' (Gabel (Blatt Niete) Treffer (Blatt Niete))))) Treffer Niete @?= LT,
      testCase "analysiere'' Test 3" $
        analysiere'' (Kopf' (Gabel (Blatt Treffer) Niete (Blatt Niete)) (Kopf' (Blatt Treffer) (Schluss' (Gabel (Blatt Niete) Treffer (Blatt Niete))))) Treffer Treffer @?= EQ,
      testCase "analysiere'' Test 4" $
        analysiere'' (Kopf' (Gabel (Blatt Treffer) Niete (Blatt Niete)) (Kopf' (Blatt Treffer) (Schluss' (Gabel (Blatt Niete) Treffer (Blatt Niete))))) Freilos Niete @?= LT,
      testCase "analysiere'' Test 5" $
        analysiere'' (Kopf' (Gabel (Blatt Treffer) Niete (Blatt Niete)) (Kopf' (Blatt Treffer) (Schluss' (Gabel (Blatt Niete) Treffer (Blatt Niete))))) Freilos Treffer @?= LT,
      testCase "analysiere'' Test 6" $
        analysiere'' (Kopf' (Gabel (Blatt Treffer) Niete (Blatt Niete)) (Kopf' (Blatt Treffer) (Schluss' (Gabel (Blatt Freilos) Treffer (Blatt Niete))))) Niete Treffer @?= EQ,
      testCase "analysiere'' test recursive" $
        let a = Gabel a Niete a in analysiere'' (Schluss' a) Niete Niete @?= EQ
    ]

analysiere'''Test :: TestTree
analysiere'''Test =
  testGroup
    "analysiere''' Tests"
    [ testCase "analysiere''' Test 0" $
        analysiere''' (Gabel' (Gabel' (Blatt' (Kopf Treffer (Schluss Niete))) (Schluss Niete) (Blatt' (Schluss Niete))) (Schluss Treffer) (Blatt' (Schluss Niete))) Niete Treffer @?= GT,
      testCase "analysiere''' Test 2" $
        analysiere''' (Gabel' (Gabel' (Blatt' (Kopf Treffer (Schluss Niete))) (Schluss Niete) (Blatt' (Schluss Niete))) (Schluss Treffer) (Blatt' (Schluss Niete))) Niete Niete @?= EQ,
      testCase "analysiere''' Test 3" $
        analysiere''' (Gabel' (Gabel' (Blatt' (Kopf Treffer (Schluss Niete))) (Schluss Niete) (Blatt' (Schluss Niete))) (Schluss Treffer) (Blatt' (Schluss Niete))) Treffer Treffer @?= EQ,
      testCase "analysiere''' Test 4" $
        analysiere''' (Gabel' (Gabel' (Blatt' (Kopf Treffer (Schluss Niete))) (Schluss Niete) (Blatt' (Schluss Niete))) (Schluss Treffer) (Blatt' (Schluss Niete))) Treffer Niete @?= LT,
      testCase "analysiere''' Test 5" $
        analysiere''' (Gabel' (Gabel' (Blatt' (Kopf Treffer (Schluss Niete))) (Schluss Niete) (Blatt' (Schluss Niete))) (Schluss Treffer) (Blatt' (Schluss Niete))) Treffer Freilos @?= GT,
      testCase "analysiere''' Test 6" $
        analysiere''' (Gabel' (Gabel' (Blatt' (Kopf Treffer (Schluss Niete))) (Schluss Niete) (Blatt' (Schluss Niete))) (Schluss Treffer) (Blatt' (Schluss Niete))) Freilos Freilos @?= EQ,
      testCase "analysiere''' Test 7" $
        analysiere''' (Gabel' (Gabel' (Blatt' (Kopf Treffer (Schluss Niete))) (Schluss Niete) (Blatt' (Schluss Niete))) (Schluss Treffer) (Blatt' (Schluss Niete))) Freilos Niete @?= LT,
      testCase "analysiere''' Test 8" $
        analysiere''' (Gabel' (Gabel' (Blatt' (Kopf Treffer (Schluss Niete))) (Schluss Niete) (Blatt' (Schluss Niete))) (Schluss Treffer) (Blatt' (Schluss Niete))) Niete Freilos @?= GT,
      testCase "analysiere''' Test 9" $
        analysiere''' (Gabel' (Gabel' (Blatt' (Kopf Treffer (Schluss Niete))) (Schluss Freilos) (Blatt' (Schluss Niete))) (Schluss Treffer) (Blatt' (Schluss Freilos))) Niete Freilos @?= EQ,
      testCase "analysiere''' recursive" $
        let a = Kopf Niete a in analysiere''' (Blatt' a) Niete Niete @?= EQ
    ]
