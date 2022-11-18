{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Angabe4_TestSuite where

import Angabe4 hiding (
  ds1,
  ds2,
  ds3,
  guenstigste_Lieferanten_Tests,
  guenstigste_Lieferanten_im_Lieferfenster_Tests,
  main,
  shouldBe,
  shouldBeList,
  shouldMList,
  sofort_erhaeltlich_bei_Tests,
  sofort_erhaeltliche_Stueckzahl_Tests,
  spec,
  toInt,
  toStr,
 )
import qualified Control.Monad as M
import Data.Function (on)
import Data.List
import Data.Ord
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "Angabe4_TestSuite Spec"
    [ sofort_erhaeltlich_bei_Tests
    , sofort_erhaeltliche_Stueckzahl_Tests
    , guenstigste_Lieferanten_Tests
    , guenstigste_Lieferanten_im_Lieferfenster_Tests
    ]

sofort_erhaeltlich_bei_Tests :: TestTree
sofort_erhaeltlich_bei_Tests =
  testGroup
    "Sofort erhältlich tests"
    [ testCase "Produkt in keinem Handel erhältlich" $ do
        sofort_erhaeltlich_bei
          (WM WM_Typ1)
          ( \n -> case n of
              _ -> haraldSortiment
          )
          `shouldBeList` []
    , testCase "Produkt im Handel erhältlich" $ do
        sofort_erhaeltlich_bei
          (WM WM_Typ3)
          ( \n -> case n of
              L1 -> bibiSortiment
              L3 -> kurtSortiment
              _ -> haraldSortiment
          )
          `shouldBeList` [L1, L3]
    , testCase "Produkt einmal im Handel erhältlich" $ do
        sofort_erhaeltlich_bei
          (WM WM_Typ2)
          ( \n -> case n of
              L5 -> johnSortiment
              _ -> haraldSortiment
          )
          `shouldBeList` [L5]
    ]

sofort_erhaeltliche_Stueckzahl_Tests :: TestTree
sofort_erhaeltliche_Stueckzahl_Tests =
  testGroup
    "Sofort erhältlich Stückzahl"
    [ testCase "Produkt in keinem Handel erhältlich" $ do
        sofort_erhaeltliche_Stueckzahl
          (WM WM_Typ1)
          ( \n -> case n of
              _ -> haraldSortiment
          )
          @?= (0, 0)
    , testCase "Produkt im Handel erhältlich" $ do
        sofort_erhaeltliche_Stueckzahl
          (WM WM_Typ3)
          ( \n -> case n of
              L1 -> bibiSortiment
              L3 -> kurtSortiment
              _ -> haraldSortiment
          )
          @?= (1 + 10, 1200 + 10 * 500)
    , testCase "Produkt einmal im Handel erhältlich" $ do
        sofort_erhaeltliche_Stueckzahl
          (WM WM_Typ2)
          ( \n -> case n of
              L5 -> johnSortiment
              _ -> haraldSortiment
          )
          @?= (5, 2800)
    ]

guenstigste_Lieferanten_Tests :: TestTree
guenstigste_Lieferanten_Tests =
  testGroup
    "Günstigster Handel"
    [ testCase "Produkt in keinem Handel erhältlich" $ do
        guenstigste_Lieferanten
          (WM WM_Typ1)
          (LF Q3 2023)
          ( \n -> case n of
              _ -> haraldSortiment
          )
          `shouldBeMList` Nothing
    , testCase "Produkt einmal im Handel erhältlich" $ do
        guenstigste_Lieferanten
          (WM WM_Typ3)
          (LF Q4 2022)
          ( \n -> case n of
              L1 -> bibiSortiment
              L3 -> kurtSortiment
              _ -> haraldSortiment
          )
          `shouldBeMList` Just [L3]
    , testCase "Produkt im Handel erhältlich" $ do
        guenstigste_Lieferanten
          (WM WM_Typ2)
          (LF Q3 2023)
          ( \n -> case n of
              L1 -> bibiSortiment
              L5 -> johnSortiment
              -- bibi kann liefern, ist aber deutlich teurer
              _ -> haraldSortiment
          )
          `shouldBeMList` Just [L5]
    ]

guenstigste_Lieferanten_im_Lieferfenster_Tests :: TestTree
guenstigste_Lieferanten_im_Lieferfenster_Tests =
  testGroup
    "Günstigst mit Rabatt"
    [ testCase "Günstigster Handel" $ do
        let ergebnis =
              guenstigste_Lieferanten_im_Lieferfenster
                (WM WM_Typ2)
                (LF Q2 2023)
                5
                ( \n -> case n of
                    L1 -> bibiSortiment
                    L5 -> johnSortiment
                    _ -> haraldSortiment
                )
        map fst ergebnis `shouldBeList` [L5]
        map (euro . snd) ergebnis @?= [2660]
    ]

johnSortiment =
  WMS
    { wm = \ty -> case ty of
        WM_Typ1 -> ds1
        WM_Typ2 -> ds2
        _ -> Nicht_im_Sortiment
    }

haraldSortiment =
  WTS
    { wt = \ty -> case ty of
        WT_Typ3 -> ds3
        WT_Typ4 -> ds1
        _ -> Nicht_im_Sortiment
    }

bibiSortiment =
  WMS
    { wm = \ty -> case ty of
        WM_Typ2 -> ds3
        WM_Typ3 -> ds3
        WM_Typ4 -> ds1
        _ -> Nicht_im_Sortiment
    }

kurtSortiment =
  WMS
    { wm = \ty -> case ty of
        WM_Typ3 -> ds1
        WM_Typ4 -> ds2
        _ -> Nicht_im_Sortiment
    }

markusSortiment =
  WSS
    { ws = \ty -> case ty of
        WS_Typ1 -> ds2
        WS_Typ3 -> ds3
        _ -> Nicht_im_Sortiment
    }

ds1 :: Datensatz
ds1 =
  DS
    { preis_in_euro = 500
    , sofort_lieferbare_stueckzahl = 10
    , -- immer lieferbar
      lieferbare_stueckzahl_im_Zeitfenster = \_ -> 10
    , skonto = DreiProzent
    }

ds2 :: Datensatz
ds2 =
  DS
    { preis_in_euro = 560
    , sofort_lieferbare_stueckzahl = 5
    , -- schwer lieferbar im Herbst
      lieferbare_stueckzahl_im_Zeitfenster = \x -> case x of
        y | quartal y == Q3 -> 1
        _ -> 5
    , skonto = FuenfProzent
    }

ds3 :: Datensatz
ds3 =
  DS
    { preis_in_euro = 1200
    , sofort_lieferbare_stueckzahl = 1
    , -- nicht dieses jahr lieferbar
      lieferbare_stueckzahl_im_Zeitfenster = \x -> case x of
        y | jahr y == 2022 -> 0
        _ -> 1
    , skonto = Kein_Skonto
    }

-- ---------------------------------------------------------------------
-- Helper functions because Lieferantenname is not
-- guaranteed Enum or Bounded or Show or Eq...
-- ---------------------------------------------------------------------

shouldBe :: Lieferantenname -> Lieferantenname -> Assertion
shouldBe l1 l2 =
  if toInt l1 /= toInt l2
    then assertFailure $ "Expected: " ++ toStr l2 ++ ", but got: " ++ toStr l1
    else pure ()

shouldBeList :: [Lieferantenname] -> [Lieferantenname] -> Assertion
shouldBeList l1 l2 = M.sequence_ $ zipWith shouldBe l1 l2

shouldBeMList :: Maybe Lieferantenliste -> Maybe Lieferantenliste -> Assertion
shouldBeMList l1 l2 = case (l1, l2) of
  (Nothing, Nothing) -> pure ()
  (Just l, Nothing) -> assertFailure $ "Expected: Nothing, but got: Just " ++ show (fmap toStr l)
  (Nothing, Just l) -> assertFailure $ "Expected: Just " ++ show (fmap toStr l) ++ " , but got: Nothing"
  (Just m, Just n) -> shouldBeList m n

infixl 1 `shouldBe`
infixl 1 `shouldBeList`
infixl 1 `shouldBeMList`

toInt L1 = 1
toInt L2 = 2
toInt L3 = 3
toInt L4 = 4
toInt L5 = 5
toInt L6 = 6
toInt L7 = 7
toInt L8 = 8
toInt L9 = 9
toInt L10 = 10

toStr L1 = "L1"
toStr L2 = "L2"
toStr L3 = "L3"
toStr L4 = "L4"
toStr L5 = "L5"
toStr L6 = "L6"
toStr L7 = "L7"
toStr L8 = "L8"
toStr L9 = "L9"
toStr L10 = "L10"
