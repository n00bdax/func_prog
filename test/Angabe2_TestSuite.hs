{-# LANGUAGE FlexibleInstances #-}

module Angabe2_TestSuite where

import Angabe2 hiding
  ( concat,
    einzulassen_Tests,
    einzulassende_Tests,
    einzulassende_abzuweisende_Tests,
    main,
    show_Tests,
    spec,
    testCase,
    testGroup,
  )
import Data.List (sort)
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)


main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "Angabe2_TestSuite"
    [ a1_Tests,
      a2_Tests,
      a3_Tests,
      a4_Tests
    ]

a1_Tests :: TestTree
a1_Tests =
  testGroup
    "Tests für Aufgabe A.1"
    [ testCase "einstellige Liste 1" $
        analysiere (Schluss Niete) Freilos Niete @?= LT,
      testCase "einstellige Liste 2" $
        analysiere (Schluss Niete) Freilos Treffer @?= EQ,
      testCase "Liste 1" $
        analysiere (Kopf Treffer $ Schluss Niete) Niete Freilos @?= GT,
      testCase "Liste 2" $
        analysiere (Kopf Freilos $ Schluss Freilos) Freilos Niete @?= GT,
      testCase "Liste 3" $
        analysiere (Kopf Freilos $ Schluss Treffer) Treffer Freilos @?= EQ
    ]

a2_Tests :: TestTree
a2_Tests =
  testGroup
    "Tests für Aufgabe A.2"
    [ testCase "blatt 1" $
        analysiere' (Blatt Niete) Freilos Niete @?= LT,
      testCase "blatt 2" $
        analysiere' (Blatt Niete) Freilos Treffer @?= EQ,
      testCase "baum 1" $
        analysiere' (Gabel (Blatt Treffer) Treffer (Blatt Niete)) Niete Freilos @?= GT,
      testCase "baum 2" $
        analysiere' (Gabel (Blatt Freilos) Freilos $ Blatt Freilos) Freilos Niete @?= GT
    ]

a3_Tests :: TestTree
a3_Tests =
  testGroup
    "Tests für Aufgabe A.3"
    [ testCase "Liste 1" $
        analysiere'' (Schluss' (Blatt Niete)) Freilos Niete @?= LT,
      testCase "Liste 2" $
        analysiere'' (Kopf' (Blatt Niete) (Schluss' (Gabel (Blatt Freilos) Freilos (Blatt Niete)))) Freilos Niete @?= EQ
    ]

a4_Tests :: TestTree
a4_Tests =
  testGroup
    "Tests für Aufgabe A.4"
    [ testCase "baum 1" $
        analysiere''' (Gabel' (Blatt' (Schluss Treffer)) (Schluss Treffer) $ Blatt' (Schluss Treffer)) Niete Freilos @?= EQ,
      testCase "baum 2" $
        analysiere'''
          ( Gabel'
              ( Gabel'
                  (Gabel' (Blatt' $ Schluss Freilos) (Kopf Freilos $ Schluss Freilos) $ Blatt' (Schluss Freilos))
                  (Schluss Freilos)
                  (Blatt' (Schluss Freilos))
              )
              (Schluss Freilos)
              $ Blatt' (Schluss Freilos)
          )
          Niete
          Freilos
          @?= LT
    ]
