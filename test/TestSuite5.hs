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
| depending on cabal/GHC
|

| To execute the test with stack *locally* execute:

stack ghci --package base --package tasty --package tasty-hunit
:l Assignment0_TestSuite1.hs
main

| Please note, your submission must be located right next to the
| test-suite on your filesystem.

-}

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module TestSuite5 where

import           Angabe5
import           Control.Exception                      (ErrorCall (ErrorCallWithLocation),
                                                         evaluate, try)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main =
  defaultMainWithIngredients
    [consoleTestReporter]
    spec

getMsg :: ErrorCall -> String
getMsg (ErrorCallWithLocation msg _) = msg

assertError :: (Show a) => String -> a -> IO ()
assertError errorMsg action = do
  r <- try (evaluate action)
  case r of
    Left e ->
      if getMsg e == errorMsg
        then return ()
        else assertFailure $ "Received unexpected error: " ++ show e ++ "\ninstead of: " ++ errorMsg
    Right _ -> assertFailure $ "Expected error: " ++ errorMsg

---------------------------------------------------------
---------------------------------------------------------

anb1, anb2, anbf1, anbf2, anbf3 :: Anbieter
anb1 =
  A
    [ (H9, sort9), -- shuffled order
      (H10, sort10),
      (H8, sort8),
      (H7, sort7),
      (H6, sort6),
      (H5, sort5),
      (H4, sort4),
      (H3, sort3),
      (H1, sort1),
      (H2, sort2)
    ]
anb2 =
  A
    [ (H1, sort10),
      (H2, sort5),
      (H3, sort3)
    ]
anbf1 =
  A
    [ (H1, sort1),
      (H2, sort2),
      (H3, sort3),
      (H4, sort4),
      (H5, sort5),
      (H6, sort6),
      (H6, sort6), -- duplicate
      (H7, sort7),
      (H8, sort8),
      (H9, sort9),
      (H10, sort10)
    ]
anbf2 =
  A
    [ (H1, sort1),
      (H2, sort2),
      (H3, sort3),
      (H4, sort4),
      (H5, sort5),
      (H6, sortf1), -- faulty Sortiment
      (H7, sort7),
      (H8, sort8),
      (H9, sort9),
      (H10, sort10)
    ]
anbf3 =
  A
    [ (H1, sort1),
      (H2, sort2),
      (H3, sort3),
      (H4, sort4),
      (H5, sort5),
      (H6, sortf3), -- faulty Sortiment
      (H7, sort7),
      (H8, sort8),
      (H9, sort9),
      (H10, sort10)
    ]

sort1, sort2, sort3, sort4, sort5, sort6, sort7, sort8, sort9, sort10, sortf1, sortf2, sortf3 :: Sortiment
sort1 = Sort [(M M2, ds1), (M M3, ds1), (M M4, ds1), (M M5, ds1), (T T2, ds1), (T T3, ds1), (T T4, ds1), (S S2, ds1), (S S3, ds0)]
sort2 = Sort [(M M1, ds2), (M M2, ds2), (M M3, ds2), (M M4, ds2), (M M5, ds2)]
sort3 = Sort [(T T1, ds2), (T T2, ds2), (T T3, ds2), (T T4, ds2)]
sort4 = Sort [(S S1, ds2), (S S2, ds2), (S S3, ds4)]
sort5 = Sort [(M M1, ds2), (T T1, ds2), (S S1, ds2)]
sort6 = Sort [(M M2, ds3), (T T2, ds3), (S S2, ds3)]
sort7 = Sort [(M M3, ds3), (T T3, ds3)]
sort8 = Sort [(M M4, ds3), (T T4, ds3)]
sort9 = Sort [(M M5, ds3)]
sort10 = Sort [(M M5, ds2), (T T4, ds2), (S S3, ds4)]
sortf1 = Sort [(M M5, ds0), (M M1, ds0), (M M5, ds0)] -- 2* M M5
sortf2 = Sort [(M M1, ds0), (M M2, ds0), (M M3, ds0), (M M4, ds0), (M M5, ds0), (T T1, ds0), (T T2, ds0), (T T3, ds0), (T T3, ds0), (T T4, ds0), (S S1, ds0), (S S2, ds0), (S S3, ds0)] -- 2* T t3
sortf3 = Sort [(M M5, dsf)] -- faulty Lieferausblick

ds0, ds1, ds2, ds3, ds4, ds5, dsf :: Datensatz
ds0 = DS 0 0 lab0 Kein_Skonto
ds1 = DS 201 2 lab1 ZehnProzent
ds2 = DS 103 3 lab2 FuenfProzent
ds3 = DS 99 4 lab3 FuenfProzent
ds4 = DS 102 0 lab4 DreiProzent
ds5 = DS 99 6 lab5 Kein_Skonto
dsf = DS 2 100 labf Kein_Skonto

lab0, lab1, lab2, lab3, lab4, lab5, lab6, labf :: Lieferausblick
lab0 = LA []
lab1 =
  LA
    [ (LF Q1 2023, 10),
      (LF Q2 2023, 11),
      (LF Q3 2023, 12),
      (LF Q4 2023, 13),
      (LF Q4 2023, 13), -- duplicate
      (LF Q1 2024, 14),
      (LF Q2 2024, 15),
      (LF Q3 2024, 16),
      (LF Q4 2024, 17),
      (LF Q1 2025, 18),
      (LF Q2 2025, 19),
      (LF Q3 2025, 20),
      (LF Q4 2025, 21)
    ]
lab2 =
  LA
    [ (LF Q1 2023, 1),
      (LF Q2 2023, 3),
      (LF Q3 2023, 1),
      (LF Q4 2023, 3),
      (LF Q1 2024, 1),
      (LF Q2 2024, 2),
      (LF Q3 2024, 1),
      (LF Q4 2024, 4),
      (LF Q1 2025, 2),
      (LF Q1 2025, 2), -- duplicate
      (LF Q2 2025, 1),
      (LF Q3 2025, 1),
      (LF Q4 2025, 1)
    ]
lab3 =
  LA
    [ (LF Q1 2023, 10),
      (LF Q2 2023, 9),
      (LF Q3 2023, 8),
      (LF Q4 2023, 7),
      (LF Q1 2024, 6),
      (LF Q2 2024, 5),
      (LF Q3 2024, 4),
      (LF Q4 2024, 3),
      (LF Q1 2025, 2),
      (LF Q1 2025, 2), -- duplicate
      (LF Q1 2025, 2), -- duplicate
      (LF Q2 2025, 1),
      (LF Q3 2025, 0),
      (LF Q4 2025, 0)
    ]
lab4 =
  LA
    [ (LF Q1 2023, 0),
      (LF Q2 2023, 0),
      (LF Q3 2023, 0),
      (LF Q4 2023, 0),
      (LF Q1 2024, 0),
      (LF Q2 2024, 0),
      (LF Q3 2024, 0),
      (LF Q4 2024, 0),
      (LF Q1 2025, 0),
      (LF Q2 2025, 0),
      (LF Q3 2025, 0),
      (LF Q4 2025, 0),
      (LF Q1 2023, 0), -- duplicate
      (LF Q2 2023, 0), -- duplicate
      (LF Q3 2023, 0), -- duplicate
      (LF Q4 2023, 0), -- duplicate
      (LF Q1 2024, 0), -- duplicate
      (LF Q2 2024, 0), -- duplicate
      (LF Q3 2024, 0), -- duplicate
      (LF Q4 2024, 0), -- duplicate
      (LF Q1 2025, 0), -- duplicate
      (LF Q2 2025, 0), -- duplicate
      (LF Q3 2025, 0), -- duplicate
      (LF Q4 2025, 0) -- duplicate
    ]
lab5 =
  LA
    [ (LF Q3 2024, 0), -- shuffled
      (LF Q3 2025, 0),
      (LF Q2 2025, 0),
      (LF Q4 2023, 0),
      (LF Q1 2023, 0),
      (LF Q2 2023, 0),
      (LF Q4 2025, 0),
      (LF Q1 2025, 0),
      (LF Q1 2024, 0),
      (LF Q2 2024, 0),
      (LF Q3 2023, 0),
      (LF Q4 2024, 0)
    ]
lab6 =
  LA
    [ (LF Q3 2024, 0), -- shuffled
      (LF Q3 2025, 0),
      (LF Q2 2025, 0),
      (LF Q4 2023, 0),
      (LF Q1 2023, 0),
      (LF Q2 2023, 0),
      (LF Q4 2025, 0),
      (LF Q1 2025, 0),
      (LF Q1 2024, 0),
      (LF Q2 2024, 0),
      (LF Q3 2023, 0),
      (LF Q4 2024, 0)
    ]
labf =
  LA
    [ (LF Q1 2023, 0),
      (LF Q2 2023, 0),
      (LF Q3 2023, 0),
      (LF Q4 2023, 0),
      (LF Q1 2024, 0),
      (LF Q2 2024, 0),
      (LF Q3 2024, 0),
      (LF Q4 2024, 0),
      (LF Q1 2025, 0),
      (LF Q2 2025, 0),
      (LF Q3 2025, 0),
      (LF Q4 2025, 0),
      (LF Q4 2023, 1) -- duplicate, differing
    ]

---------------------------------------------------------
---------------------------------------------------------

spec :: TestTree
spec =
  testGroup
    "all test5tests"
    [ testCase "[ A.1 - 1 ]" $ ist_wgf sort1 @?= True,
      testCase "[ A.1 - 2 ]" $ ist_wgf sortf2 @?= False,
      testCase "[ A.1 - 3 ]" $ ist_nwgf sortf1 @?= True,
      testCase "[ A.1 - 4 ]" $ ist_nwgf sort3 @?= False,

      testCase "[ A.1 - 5 ]" $ ist_wgf anb1 @?= True,
      testCase "[ A.1 - 6 ]" $ ist_wgf anbf1 @?= False,
      testCase "[ A.1 - 7 ]" $ ist_nwgf anbf2 @?= True,
      testCase "[ A.1 - 8 ]" $ ist_nwgf anb2 @?= False,

      testCase "[ A.1 - 9 ]"  $ ist_wgf lab6 @?= True,
      testCase "[ A.1 - 10 ]" $ ist_wgf labf @?= False,
      testCase "[ A.1 - 11 ]" $ ist_nwgf labf @?= True,
      testCase "[ A.1 - 12 ]" $ ist_nwgf lab3 @?= False,
      testCase "[ A.1 - 13 ]" $ ist_wgf lab0 @?= True,

      testCase "[ A.5 - 1 ]" $ sofort_lieferfaehig (M M1) anb1 @?= [H5, H2],
      testCase "[ A.5 - 2 ]" $ sofort_lieferfaehig (T T3) anb1 @?= [H7, H3, H1],
      testCase "[ A.5 - 3 ]" $ sofort_lieferfaehig (T T4) anb2 @?= [H3, H1],
      testCase "[ A.5 - 4 ]" $ sofort_lieferfaehig (S S3) anb2 @?= [],
      testCase "[ A.5 - 5 ]" $ sofort_lieferfaehig (T T2) anb2 @?= [H3],
      testCase "[ A.5 - 6 ]" $ sofort_lieferfaehig (M M5) anb1 @?= [H10, H9, H2, H1],
      testCase "[ A.5 - 7 ]" $ sofort_lieferfaehig (S S1) anb1 @?= [H5, H4],

      testCase "[ A.6 - 1 ]" $ sofort_erhaeltliche_Stueckzahl (M M1) anb1 @?= (6, 618),
      testCase "[ A.6 - 2 ]" $ sofort_erhaeltliche_Stueckzahl (T T3) anb1 @?= (9, 1107),
      testCase "[ A.6 - 3 ]" $ sofort_erhaeltliche_Stueckzahl (T T4) anb2 @?= (6, 618),
      testCase "[ A.6 - 4 ]" $ sofort_erhaeltliche_Stueckzahl (S S3) anb2 @?= (0, 0),
      testCase "[ A.6 - 5 ]" $ sofort_erhaeltliche_Stueckzahl (T T2) anb2 @?= (3, 309),
      testCase "[ A.6 - 6 ]" $ sofort_erhaeltliche_Stueckzahl (M M5) anb1 @?= (12, 1416),
      testCase "[ A.6 - 7 ]" $ sofort_erhaeltliche_Stueckzahl (S S1) anb1 @?= (6, 618),

      testCase "[ A.7 - 1 ]" $ guenstigste_Lieferanten (M M1) (LF Q1 2023) anb1 @?= Just [H5, H2],
      testCase "[ A.7 - 2 ]" $ guenstigste_Lieferanten (T T3) (LF Q2 2023) anb1 @?= Just [H7],
      testCase "[ A.7 - 3 ]" $ guenstigste_Lieferanten (T T4) (LF Q3 2023) anb2 @?= Just [H3, H1],
      testCase "[ A.7 - 4 ]" $ guenstigste_Lieferanten (S S3) (LF Q4 2023) anb2 @?= Nothing,
      testCase "[ A.7 - 5 ]" $ guenstigste_Lieferanten (T T2) (LF Q1 2024) anb2 @?= Just [H3],
      testCase "[ A.7 - 6 ]" $ guenstigste_Lieferanten (M M5) (LF Q2 2024) anb1 @?= Just [H9],
      testCase "[ A.7 - 7 ]" $ guenstigste_Lieferanten (S S1) (LF Q1 2050) anb1 @?= Nothing,

      testCase "[ A.8 - 1 ]" $ guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 1 anb1 @?= [(H5, EUR {euro = 100}), (H2, EUR {euro = 100})],
      testCase "[ A.8 - 2 ]" $ guenstigste_Lieferanten_im_Lieferfenster (T T3) (LF Q2 2023) 2 anb1 @?= [(H7, EUR {euro = 190})],
      testCase "[ A.8 - 3 ]" $ guenstigste_Lieferanten_im_Lieferfenster (T T4) (LF Q3 2023) 3 anb2 @?= [],
      testCase "[ A.8 - 4 ]" $ guenstigste_Lieferanten_im_Lieferfenster (S S3) (LF Q4 2023) 4 anb2 @?= [],
      testCase "[ A.8 - 5 ]" $ guenstigste_Lieferanten_im_Lieferfenster (T T3) (LF Q1 2024) 5 anb1 @?= [(H7, EUR {euro = 480})],
      testCase "[ A.8 - 6 ]" $ guenstigste_Lieferanten_im_Lieferfenster (M M5) (LF Q2 2024) 6 anb1 @?= [(H1, EUR 1090)],
      testCase "[ A.8 - 7 ]" $ guenstigste_Lieferanten_im_Lieferfenster (S S1) (LF Q1 2050) 7 anb1 @?= [],

      testCase "[ A.5 - f1 ]" $ assertError "Anbieterargumentfehler" (sofort_lieferfaehig (M M1) anbf1),
      testCase "[ A.5 - f2 ]" $ assertError "Anbieterargumentfehler" (sofort_lieferfaehig (T T4) anbf2),
      testCase "[ A.5 - f3 ]" $ assertError "Anbieterargumentfehler" (sofort_lieferfaehig (T T2) anbf3),

      testCase "[ A.6 - f1 ]" $ assertError "Anbieterargumentfehler" (sofort_erhaeltliche_Stueckzahl (S S3) anbf1),
      testCase "[ A.6 - f2 ]" $ assertError "Anbieterargumentfehler" (sofort_erhaeltliche_Stueckzahl (M M2) anbf2),
      testCase "[ A.6 - f3 ]" $ assertError "Anbieterargumentfehler" (sofort_erhaeltliche_Stueckzahl (M M4) anbf3),

      testCase "[ A.7 - f1 ]" $ assertError "Anbieterargumentfehler" (guenstigste_Lieferanten (M M5) (LF Q1 2023) anbf1),
      testCase "[ A.7 - f2 ]" $ assertError "Anbieterargumentfehler" (guenstigste_Lieferanten (M M5) (LF Q1 2023) anbf2),
      testCase "[ A.7 - f3 ]" $ assertError "Anbieterargumentfehler" (guenstigste_Lieferanten (M M1) (LF Q1 2023) anbf3),

      testCase "[ A.8 - f1 ]" $ assertError "Anbieterargumentfehler" (guenstigste_Lieferanten_im_Lieferfenster (M M5) (LF Q1 2023) 0 anbf1),
      testCase "[ A.8 - f2 ]" $ assertError "Anbieterargumentfehler" (guenstigste_Lieferanten_im_Lieferfenster (M M5) (LF Q1 2023) 0 anbf2),
      testCase "[ A.8 - f3 ]" $ assertError "Anbieterargumentfehler" (guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 1 anbf3),
      testCase "True == True" $ True @?= True
    ]
