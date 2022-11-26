{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Angabe5_TestSuite where

import Angabe5 hiding
  ( MyToString,
    guenstigste_Lieferanten_im_Lieferfenster_tests,
    guenstigste_Lieferanten_tests,
    main,
    myToString,
    sofort_erhaeltliche_Stueckzahl_tests,
    sofort_lieferfaehig_tests,
    sort1,
    sort2,
    sort3,
    spec,
    wohlgeformt_tests,
  )
import qualified Control.Exception as Exc
import Data.List (sort, sortBy)
import Test.Tasty as T
  ( TestTree,
    defaultMainWithIngredients,
    testGroup,
  )
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "Angabe5_TestSuite Spec"
    [ wohlgeformt_tests,
      sofort_lieferfaehig_tests,
      sofort_erhaeltliche_Stueckzahl_tests,
      guenstigste_Lieferanten_tests,
      guenstigste_Lieferanten_im_Lieferfenster_tests
    ]

sort1 :: Sortiment
sort1 =
  Sort
    [ (M M1, DS 100 1 (LA [(LF Q1 2023, 1)]) Kein_Skonto),
      (T T2, DS 100 0 (LA [(LF Q2 2024, 1)]) Kein_Skonto)
    ]

sort2 :: Sortiment
sort2 =
  Sort
    [ (S S3, DS 100 1 (LA []) Kein_Skonto),
      (T T2, DS 200 2 (LA [(LF Q4 2023, 1), (LF Q4 2024, 1)]) ZehnProzent)
    ]

sort3 :: Sortiment
sort3 =
  Sort
    [(T T2, DS 190 1 (LA [(LF Q4 2023, 2)]) Kein_Skonto)]

-- A1 --
wohlgeformt_tests :: TestTree
wohlgeformt_tests =
  testGroup
    "wohlgeformt"
    [ testCase "wgf Lieferausblick 1" $
        ist_wgf (LA []) @?= True,
      testCase "wgf Lieferausblick 2" $
        ist_wgf (LA [(LF Q1 2023, 1), (LF Q2 2024, 3)]) @?= True,
      testCase "wgf Lieferausblick 3" $
        ist_wgf (LA [(LF Q2 2023, 3), (LF Q2 2023, 3)]) @?= True,
      testCase "wgf Lieferausblick 4" $
        ist_wgf (LA [(LF Q2 2023, 3), (LF Q2 2023, 4)]) @?= False,
      testCase "wgf Sortiment 1" $
        ist_wgf (Sort [(M M1, DS 1 1 (LA [(LF Q1 2023, 1)]) Kein_Skonto)]) @?= True,
      testCase "wgf Sortiment 2" $
        ist_wgf
          ( Sort
              [ (M M1, DS 1 1 (LA [(LF Q1 2023, 1)]) Kein_Skonto),
                (M M1, DS 1 1 (LA []) Kein_Skonto)
              ]
          )
          @?= False,
      testCase "wgf Anbieter 1" $
        ist_wgf (A [(H1, sort1), (H2, sort1)]) @?= True,
      testCase "wgf Anbieter 2" $
        ist_wgf
          ( A
              [ (H1, sort1),
                (H1, sort2)
              ]
          )
          @?= False
    ]

sofort_lieferfaehig_tests :: TestTree
sofort_lieferfaehig_tests =
  testGroup
    "sofort_lieferfaehig"
    [ testCase "lieferbar" $
        sofort_lieferfaehig (M M1) (A [(H4, sort1)]) `shouldBeList` [H4],
      testCase "nicht angeboten" $
        sofort_lieferfaehig (M M2) (A [(H4, sort1)]) `shouldBeList` [],
      testCase "nicht lieferbar" $
        sofort_lieferfaehig (T T2) (A [(H4, sort1)]) `shouldBeList` []
    ]

sofort_erhaeltliche_Stueckzahl_tests :: TestTree
sofort_erhaeltliche_Stueckzahl_tests =
  testGroup
    "sofort_erhaeltliche_Stueckzahl"
    [ testCase "nicht erhältlich" $
        sofort_erhaeltliche_Stueckzahl (T T2) (A [(H3, sort1)]) @?= (0, 0),
      testCase "verfügbar 1" $
        sofort_erhaeltliche_Stueckzahl (T T2) (A [(H3, sort2)]) @?= (2, 400),
      testCase "verfügbar 2" $
        sofort_erhaeltliche_Stueckzahl (T T2) (A [(H3, sort2), (H4, sort3)]) @?= (3, 590)
    ]

guenstigste_Lieferanten_tests :: TestTree
guenstigste_Lieferanten_tests =
  testGroup
    "guenstigste_Lieferanten"
    [ testCase "nicht erhältlich" $
        guenstigste_Lieferanten (M M4) (LF Q2 2024) (A [(H3, sort1), (H2, sort2), (H1, sort3)]) `shouldBeFF` Nothing,
      testCase "nicht erhältlich im Fenster" $
        guenstigste_Lieferanten (T T2) (LF Q2 2035) (A [(H3, sort1), (H2, sort2), (H1, sort3)]) `shouldBeFF` Nothing,
      testCase "verfügbar 1" $
        guenstigste_Lieferanten (T T2) (LF Q4 2024) (A [(H3, sort2), (H4, sort3)]) `shouldBeFF` Just [H3],
      testCase "verfügbar 2" $
        guenstigste_Lieferanten (T T2) (LF Q4 2023) (A [(H3, sort2), (H4, sort3)]) `shouldBeFF` Just [H4]
    ]

guenstigste_Lieferanten_im_Lieferfenster_tests :: TestTree
guenstigste_Lieferanten_im_Lieferfenster_tests =
  testGroup
    "guenstigste_Lieferanten_im_Lieferfenster"
    [ testCase "nicht erhältlich" $
        guenstigste_Lieferanten_im_Lieferfenster (M M4) (LF Q2 2024) 1 (A [(H3, sort1), (H2, sort2), (H1, sort3)]) `shouldBeList` [],
      testCase "nicht erhältlich im Fenster" $
        guenstigste_Lieferanten_im_Lieferfenster (T T2) (LF Q2 2035) 1 (A [(H3, sort1), (H2, sort2), (H1, sort3)]) `shouldBeList` [],
      testCase "verfügbar 1" $
        guenstigste_Lieferanten_im_Lieferfenster (T T2) (LF Q4 2024) 1 (A [(H3, sort2), (H4, sort3)]) `shouldBeList` [(H3, EUR 180)],
      testCase "verfügbar, das günstigste (nach skonto) wird genommen" $
        guenstigste_Lieferanten_im_Lieferfenster (T T2) (LF Q4 2023) 1 (A [(H3, sort2), (H4, sort3)]) `shouldBeList` [(H3, EUR 180)],
      testCase "zu viele für H3 angefragt" $
        guenstigste_Lieferanten_im_Lieferfenster (T T2) (LF Q4 2023) 2 (A [(H3, sort2), (H4, sort3)]) `shouldBeList` [(H4, EUR 380)]
    ]

infixl 1 `shouldBe`

infixl 1 `shouldBeList`

shouldBe :: MyToString a => a -> a -> Assertion
shouldBe a b = myToString a @?= myToString b

shouldBeList :: MyToString a => [a] -> [a] -> Assertion
shouldBeList = shouldBeF

shouldBeF :: (Functor t, MyToString a, Eq (t String), Show (t String)) => t a -> t a -> Assertion
shouldBeF a b = fmap myToString a @?= fmap myToString b

shouldBeFF ::
  (Functor t1, Functor t2, MyToString a, Eq (t1 (t2 String)), Show (t1 (t2 String))) =>
  t1 (t2 a) ->
  t1 (t2 a) ->
  Assertion
shouldBeFF a b = fmap (fmap myToString) a @?= fmap (fmap myToString) b

-- missing deriving instances :/
class MyToString a where
  myToString :: a -> String

instance MyToString Haendler where
  myToString H1 = "H1"
  myToString H2 = "H2"
  myToString H3 = "H3"
  myToString H4 = "H4"
  myToString H5 = "H5"
  myToString H6 = "H6"
  myToString H7 = "H7"
  myToString H8 = "H8"
  myToString H9 = "H9"
  myToString H10 = "H10"

instance MyToString EUR where
  myToString (EUR e) = show e

instance (MyToString a, MyToString b) => MyToString (a, b) where
  myToString (a, b) = "(" ++ myToString a ++ "," ++ myToString b ++ ")"
