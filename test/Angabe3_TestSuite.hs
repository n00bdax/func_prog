{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Angabe3_TestSuite where

import Angabe3 hiding (main, zeileToList, toLists, fromLists, zeileFromList)
import qualified Control.Exception as Exc
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Prelude as P

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "Angabe3_TestSuite Spec"
    [ matrixTyp_Tests
    , eq_Tests
    , num_Tests
    ]

matrixTyp_Tests :: TestTree
matrixTyp_Tests =
  testGroup
    "Tests für matrixtyp"
    [ testCase "Leere Matrix" $ do
        matrixtyp (fromLists [[1, 2, 3], [1, 2]]) @?= KeineMatrix
    , testCase "2x3 Matrix" $ do
        matrixtyp (fromLists [[1 .. 3], [4 .. 6]]) @?= Matrix_vom_Typ (2, 3)
    , testCase "Nicht jede Zeile hat gleich viele Elemente" $ do
        matrixtyp (fromLists [[1 .. 3], [4 .. 6], [6 .. 9]]) @?= KeineMatrix
    , testCase "Unendlich lange Zeile" $ do
        matrixtyp (fromLists [[1 .. 3], [4], [6 ..]]) @?= KeineMatrix
    , testCase "Einzelnes Element" $ do
        matrixtyp (fromLists [[1]]) @?= Matrix_vom_Typ (1, 1)
    ]

eq_Tests :: TestTree
eq_Tests =
  testGroup
    "Tests für die Num Instanz von Matrix"
    [ testCase "Gleiche Matrizen 1" $ do
        fromLists [[1 .. 2], [3 .. 4], [5 .. 6]] P.== fromLists [[1 .. 2], [3 .. 4], [5 .. 6]] @?= True
    , testCase "Gleiche Matrizen 2" $ do
        fromLists [[1 .. 2], [3 .. 4], [5 .. 6]] P./= fromLists [[1 .. 2], [3 .. 4], [5 .. 6]] @?= False
    , testCase "Ungleiche Matrizen 1" $ do
        fromLists [[2 .. 3], [3 .. 4], [5 .. 6]] P.== fromLists [[1 .. 2], [3 .. 4], [5 .. 6]] @?= False
    , testCase "Ungleiche Matrizen 2" $ do
        fromLists [[2 .. 3], [3 .. 4], [5 .. 6]] P./= fromLists [[1 .. 2], [3 .. 4], [5 .. 6]] @?= True
    , testCase "Falsche Dimensionen 1" $ do
        expectSomeError (fromLists [[2], [3 .. 4], [5 .. 6]] P.== fromLists [[1 .. 2], [3 .. 4], [5 .. 6]])
    , testCase "Falsche Dimensionen 2" $ do
        expectSomeError (fromLists [[2], [3 .. 4], [5 .. 6]] P./= fromLists [[1 .. 2], [3 .. 4], [5 .. 6]])
    ]

num_Tests :: TestTree
num_Tests =
  testGroup
    "Tests für die Num Instanz von Matrix"
    [ testCase "Addition 1" $
        do
          fromLists [[1 .. 3], [4 .. 6], [7 .. 9]] P.+ fromLists [[9, 8, 7], [6, 5, 4], [3, 2, 1]]
          `isEqualTo` fromLists [[10, 10, 10], [10, 10, 10], [10, 10, 10]]
    , testCase "Addition 2" $ do
         expectSomeError (fromLists [[1 .. 3]] P.+ fromLists [[1], [2], [3]])
    , testCase "Subtraktion 1" $
        do
          fromLists [[1 .. 3], [4 .. 6], [7 .. 9]] P.- fromLists [[1 .. 3], [4 .. 6], [7 .. 9]]
          `isEqualTo` fromLists [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
    , testCase "Subtraktion 2" $ do
        expectSomeError (fromLists [[1 .. 3]] P.- fromLists [[1], [2], [3]])
    , testCase "abs 1" $ do
        P.abs (fromLists [[1 .. 3], [-4, -5, -6]]) `isEqualTo` fromLists [[1 .. 3], [4 .. 6]]
    , testCase "fromInteger 1" $ do
        P.fromInteger 4 `isEqualTo` fromLists [[4]]
    ]

-- Helper to check for errors
expectSomeError :: a -> Assertion
expectSomeError val = do
  res <- Exc.try (Exc.evaluate val)
  case res of
    Left (Exc.ErrorCall _) -> pure ()
    Right _ -> assertFailure "Expected error call but got none"

-- | Structural equality of the matrix. Assumes well-formed matrices.
isEqualTo :: Matrix -> Matrix -> Assertion
isEqualTo mx my = toLists mx P.== toLists my @? "Expected matrices to be equal"

infixl 1 `isEqualTo`

fromLists :: [[Int]] -> Matrix
fromLists [] = error "TestSuite3.matrixFromLists: internal error"
fromLists [xs] = LZ (zeileFromList xs)
fromLists (xs : xss) = Z (zeileFromList xs) (fromLists xss)

zeileFromList :: [Int] -> Zeile
zeileFromList [] = error "TestSuite3.zeileFromList: internal error"
zeileFromList [x] = LE x
zeileFromList (x : xs) = E x (zeileFromList xs)

zeileToList :: Zeile -> [Int]
zeileToList (LE x) = [x]
zeileToList (E x xs) = x : zeileToList xs

toLists :: Matrix -> [[Int]]
toLists (LZ xs) = [zeileToList xs]
toLists (Z xs xss) = zeileToList xs : toLists xss
