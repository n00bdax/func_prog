{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Angabe7_TestSuite where

import Angabe7 hiding (
  berichtige_tests,
  conversion2_tests,
  conversion_tests,
  ds_q1_2023,
  ds_q2_2024,
  expectSomeError,
  la_q1_2023,
  la_q2_2024,
  main,
  markt,
  preisanpassungs_tests,
  spec,
  toDs',
 )
import qualified Control.Exception as Exc
import qualified Data.List as List (sort)
import qualified Data.Maybe as Maybe (fromMaybe)
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Prelude as P

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "TestSuite7 Spec"
    [ conversion_tests
    , conversion2_tests
    , preisanpassungs_tests
    , berichtige_tests
    ]

ds_q1_2023 = DS 100 5 (LA $ \f -> case f of LF Q1 2023 -> 1; _ -> 0) Kein_Skonto
ds_q2_2024 = DS 200 10 (LA $ \f -> case f of LF Q2 2024 -> 5; _ -> 0) Kein_Skonto

la_q1_2023 = LA' [(LF Q1 2023, 1)]
la_q2_2024 = LA' [(LF Q2 2024, 1)]

toDs' :: Lieferausblick' -> Datensatz -> Datensatz'
toDs' lf Nicht_im_Sortiment = Nicht_im_Sortiment'
toDs' lf DS{..} =
  DS'
    { preis_in_euro' = preis_in_euro
    , sofort_lieferbare_stueckzahl' = sofort_lieferbare_stueckzahl
    , lieferbare_stueckzahl_im_Zeitfenster' = lf
    , skonto' = skonto
    }

-- A1
conversion_tests :: TestTree
conversion_tests =
  testGroup
    "Konvertierungstests"
    [ testCase "lst2fkt_la 1" $
        lst2fkt_la lf (LF Q2 2023) @?= 12
    , testCase "lst2fkt_la 2" $
        expectSomeError (lst2fkt_la lf (LF Q2 2025))
    , testCase "lst2fkt_la 3" $
        lst2fkt_la lf (LF Q3 2023) @?= 4
    , testCase "lst2fkt_so 1" $
        sofort_lieferbare_stueckzahl (lst2fkt_so so (M M1)) @?= 5
    , testCase "lst2fkt_so 2" $
        expectSomeError (lst2fkt_so so (S S3))
    , testCase "lst2fkt_so 3" $
        sofort_lieferbare_stueckzahl (lst2fkt_so so (T T1)) @?= 10
    , testCase "lst2fkt_ab 1" $ do
        let Sort s = lst2fkt_ab ab H3
        preis_in_euro (s (M M1)) @?= 100
    , testCase "lst2fkt_ab 2" $
        expectSomeError (lst2fkt_ab ab H5)
    , testCase "lst2fkt_ab 3" $ do
        let Sort s = lst2fkt_ab ab H10
        preis_in_euro (s (T T1)) @?= 200
    ]
 where
  lf = [(LF Q1 2023, 1), (LF Q2 2023, 12), (LF Q3 2023, 4)]
  so = [(M M1, toDs' la_q1_2023 ds_q1_2023), (T T1, toDs' la_q2_2024 ds_q2_2024)]
  ab =
    [ (H3, Sort' [(M M1, toDs' la_q1_2023 ds_q1_2023), (T T1, toDs' la_q2_2024 ds_q2_2024)])
    , (H10, Sort' [(T T1, toDs' la_q2_2024 ds_q2_2024)])
    ]

-- A2
conversion2_tests :: TestTree
conversion2_tests =
  testGroup
    "Konvertierungstests"
    [ testCase "lst2fkt_la' 1" $ do
        let LA f = lst2fkt_la' lf
        f (LF Q2 2023) @?= 12
    , testCase "lst2fkt_la' 2" $ do
        let LA f = lst2fkt_la' lf
        expectSomeError (f (LF Q2 2025))
    , testCase "lst2fkt_la' 3" $ do
        let LA f = lst2fkt_la' lf
        f (LF Q3 2023) @?= 4
    , testCase "lst2fkt_so' 1" $ do
        let Sort s = lst2fkt_so' so
        sofort_lieferbare_stueckzahl (s (M M1)) @?= 5
    , testCase "lst2fkt_so' 2" $ do
        let Sort s = lst2fkt_so' so
        expectSomeError (s (S S3))
    , testCase "lst2fkt_so' 3" $ do
        let Sort s = lst2fkt_so' so
        sofort_lieferbare_stueckzahl (s (T T1)) @?= 10
    , testCase "lst2fkt_ab' 1" $ do
        let Mt m = lst2fkt_ab' ab
        let Sort s = m H3
        preis_in_euro (s (M M1)) @?= 100
    , testCase "lst2fkt_ab' 2" $ do
        let Mt m = lst2fkt_ab' ab
        expectSomeError (m H5)
    , testCase "lst2fkt_ab' 3" $ do
        let Mt m = lst2fkt_ab' ab
        let Sort s = m H10
        preis_in_euro (s (T T1)) @?= 200
    ]
 where
  lf = LA' [(LF Q1 2023, 1), (LF Q2 2023, 12), (LF Q3 2023, 4)]
  so = Sort' [(M M1, toDs' la_q1_2023 ds_q1_2023), (T T1, toDs' la_q2_2024 ds_q2_2024)]
  ab =
    Mt'
      [ (H3, Sort' [(M M1, toDs' la_q1_2023 ds_q1_2023), (T T1, toDs' la_q2_2024 ds_q2_2024)])
      , (H10, Sort' [(T T1, toDs' la_q2_2024 ds_q2_2024)])
      ]

preisanpassungs_tests :: TestTree
preisanpassungs_tests =
  testGroup
    "Preisanpassung"
    [ testCase "Test 1" $ do
        let Mt m = preisanpassung markt
        let Sort h3 = m H3
        let Sort h10 = m H10
        preis_in_euro (h3 $ M M1) @?= 100
        preis_in_euro (h3 $ T T1) @?= 200
        preis_in_euro (h10 $ T T1) @?= 200
    ]

berichtige_tests :: TestTree
berichtige_tests =
  testGroup
    "Berichtigung"
    [ testCase "Test 1" $ do
        let Mt m =
              berichtige
                markt
                (BH $ \case H3 -> Betroffen; _ -> NichtBetroffen)
                (LF Q3 2023)
        let Sort h3 = m H3
        let Sort h10 = m H10
        let LA m1 = lieferbare_stueckzahl_im_Zeitfenster (h3 $ M M1)
        let LA h3t1 = lieferbare_stueckzahl_im_Zeitfenster (h3 $ T T1)
        let LA h10t1 = lieferbare_stueckzahl_im_Zeitfenster (h10 $ T T1)
        m1 (LF Q1 2023) @?= 1
        h3t1 (LF Q4 2023) @?= 0
        h10t1 (LF Q2 2024) @?= 5
    ]

markt :: Markt
markt = Mt $ \case
  H3 -> Sort $ \case
    M M1 -> ds_q1_2023
    T T1 -> ds_q2_2024
    _ -> Nicht_im_Sortiment
  H10 -> Sort $ \case
    T T1 -> ds_q2_2024
    _ -> Nicht_im_Sortiment
  _ -> Sort $ \case
    _ -> Nicht_im_Sortiment


-- Helper to check for errors
expectSomeError :: a -> Assertion
expectSomeError val = do
  res <- Exc.try (Exc.evaluate val)
  case res of
    Left (Exc.ErrorCall _) -> pure ()
    Right _ -> assertFailure "Expected error call but got none"
