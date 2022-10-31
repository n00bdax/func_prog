{-
 - How can this test-suite be executed?
 -
 - You can execute the test-suite on g0 via:
 -
 - > cabal repl -b base -b tasty -b tasty-hunit
 - > :l Assignment0_TestSuite1.hs
 - > main
 -
 - It works the same on you local machine.
 - To execute the test with stack *locally* execute:
 -
 - > stack ghci --package base --package tasty --package tasty-hunit
 - > :l Assignment0_TestSuite1.hs
 - > main
 -
 - Please note, your submission must be located right next to the test-suite
 - on your filesystem.
-}
{-# LANGUAGE TupleSections #-}

module TestSuite1 where

import Angabe1 hiding (main, repeat, replicate, sort)
import Data.List (sort)
import Test.Tasty (testGroup)
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main =
  defaultMainWithIngredients
    [consoleTestReporter]
    spec

spec :: TestTree
spec =
  testGroup
    "TestSuite1 Spec"
    [ haeufigkeitTests
    , gewichtTests
    , korrigiereTests
    , korrigiere'Tests
    ]

haeufigkeitTests :: TestTree
haeufigkeitTests =
  testGroup
    "Häufigkeit"
    [ testCase "Keine Eingabe" $ do
        haeufigkeit "" @?= []
    , testCase "Haskell" $
        do
          haeufigkeit "Haskell"
          @?= [('H', 1), ('a', 1), ('s', 1), ('k', 1), ('e', 1), ('l', 2)]
    , testCase "Langes Wort" $
        do
          haeufigkeit "Programmierung mit Haskell"
          @?= [ ('P', 1)
              , ('r', 3)
              , ('o', 1)
              , ('g', 2)
              , ('a', 2)
              , ('m', 3)
              , ('i', 2)
              , ('e', 2)
              , ('u', 1)
              , ('n', 1)
              , (' ', 2)
              , ('t', 1)
              , ('H', 1)
              , ('s', 1)
              , ('k', 1)
              , ('l', 2)
              ]
    , testCase "Stress Test" $ do
        haeufigkeit (replicate 1000000 'a') @?= [('a', 1000000)]
    ]

gewichtTests :: TestTree
gewichtTests =
  testGroup
    "Gewicht"
    [ testCase "Gewichtssberechnung" $ do
        gewicht "Haskell" [('l', 9), ('k', 14), ('H', 10), ('a', 2)] @?= 44
    , testCase "Kleinere Gewichtsberechnung" $ do
        gewicht "Haskell" [('b', 4), ('a', 25), ('H', 0)] @?= 25
    , testCase "Kein Verzeichnis" $ do
        gewicht "Java" [] @?= 0
    , testCase "Fehlerhaftes Verzeichnis" $ do
        gewicht "Haskell" [('H', 10), ('k', 14), ('k', 3), ('a', 2), ('l', 8), ('a', 9)] @?= (-1)
    ]

korrigiereTests :: TestTree
korrigiereTests =
  testGroup
    "Korrigiere"
    [ testCase "Korrektes Verzeichnis" $
        do
          korrigiere [('a', 2), ('k', 14), ('H', 10), ('l', 8)]
          @?= [('a', 2), ('k', 14), ('H', 10), ('l', 8)]
    , testCase "Korrigiere" $ do
        korrigiere [('a', 2), ('k', 14), ('H', 10), ('k', 3), ('l', 8), ('a', 9)]
          @?= [('a', 2), ('k', 14), ('H', 10), ('l', 8)]
    , testCase "Korrigiere 2" $ do
        korrigiere (map ('a',) [1 .. 5] ++ [('b', 3), ('b', 4)] ++ [('H', k) | k <- [10 .. 12]])
          @?= [('a', 1), ('b', 3), ('H', 10)]
    , testCase "Kein Verzeichnis" $ korrigiere [] @?= []
    ]

korrigiere'Tests :: TestTree
korrigiere'Tests =
  testGroup
    "Korrigiere'"
    [ testCase "Korrektes Verzeichnis" $
        do
          korrigiere' [('a', 2), ('k', 14), ('H', 10), ('l', 8)]
          @?= [('a', 2), ('k', 14), ('H', 10), ('l', 8)]
    , testCase "Korrigiere'" $ do
        korrigiere' [('a', 2), ('k', 14), ('H', 10), ('k', 3), ('l', 8), ('a', 9)]
          @?= [('a', 11), ('k', 17), ('H', 10), ('l', 8)]
    , testCase "Korrigiere' 2" $ do
        korrigiere' (map ('a',) [1 .. 5] ++ [('b', 3), ('b', 4)] ++ [('H', k) | k <- [10 .. 12]])
          @?= [('a', 15), ('b', 7), ('H', 33)]
    , testCase "Kein Verzeichnis" $ korrigiere' [] @?= []
    ]
