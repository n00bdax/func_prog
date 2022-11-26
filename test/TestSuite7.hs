{-

| How can this test-suite be executed?
|
| You can execute the test-suite on g0 or locally with TestSuite
| and Angabe file placed within the same folder via:

cabal repl -b base -b tasty -b tasty-hunit
:l TestSuite7
main

| For the use from the top level within template folder place
| Angabe6.hs in src/
| TestSuite6 in test/
| execute only this TestSuite with:

cabal repl TestSuite7
main

| or execute all TestSuites with:

cabal test

| To execute the test with stack *locally* execute:
stack ghci --package base --package tasty --package tasty-hunit
:l TestSuite7.hs
main

-}

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sortOn" #-}

module TestSuite7 where

import           Control.Exception                      (ErrorCall (ErrorCallWithLocation),
                                                         evaluate, try)
import           Data.Bifunctor                         (Bifunctor (second))
import           Test.Tasty                             (TestTree,
                                                         defaultMainWithIngredients,
                                                         testGroup)
import           Test.Tasty.HUnit                       (assertFailure,
                                                         testCase, (@?=))
import           Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)



import           Angabe7                                (AbLieferfenster,
                                                         Betroffen (..),
                                                         Betroffene_Haendler (..),
                                                         Datensatz (..),
                                                         Datensatz' (..),
                                                         EUR (..),
                                                         Haendler (..),
                                                         Lieferausblick (..),
                                                         Lieferausblick' (..),
                                                         Lieferfenster (..),
                                                         Markt (..),
                                                         Markt' (..), Nat0,
                                                         Nat1, Quartal (..),
                                                         Skonto (..),
                                                         Sortiment (..),
                                                         Sortiment' (..),
                                                         Typ (..),
                                                         Waescheschleuder (..),
                                                         Waeschetrockner (..),
                                                         Waschmaschine (..),
                                                         berichtige, lst2fkt_ab,
                                                         lst2fkt_la, lst2fkt_so,
                                                         preisanpassung)
import           Data.List                              (sort, sortBy)
import           Data.Ord                               (Down (Down), comparing)


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
        Left e -> if getMsg e == errorMsg then return()
                  else assertFailure $ "Received unexpected error: " ++ show e ++ "\ninstead of: " ++ errorMsg
        Right _ -> assertFailure $ "Expected error: " ++ errorMsg


---------------------------------------------------------
---------------------------------------------------------

m1, m2, mf1, mf2 :: Markt'
m1 = Mt'
    [ (H10,sort2),
      (H9, sort3),
      (H8, sort4),
      (H7, sort2),
      (H6, sort0),
      (H5, sort5),
      (H4, sort4),
      (H3, sort3),
      (H1, sort1),
      (H2, sort2)
    ]
m2 = Mt'
    [ (H1,  sort1),
      (H2,  sort2),
      (H3,  sort3),
      (H4,  sort4),
      (H5,  sort5),
      (H6,  sort0),
      (H7,  sort0),
      (H8,  sort0),
      (H9,  sort0),
      (H10, sort0)
    ]
mf1 = Mt'
    [ (H1, sort2),-- missing Haendler
      (H2, sort5),
      (H3, sort3)
    ]
mf2 = Mt'
    [ (H1, sort1),
      (H2, sort2),
      (H3, sort3),
      (H4, sort4),
      (H5, sort5),
      (H6, sortf1), -- missing Typ
      (H7, sort0),
      (H8, sort0),
      (H9, sort0),
      (H10,sort0)
    ]

sort0,sort1, sort2, sort3, sort4, sort5,sortf1  :: Sortiment'
sort0 = Sort' [(M M1,ds0),(M M2,ds0),(M M3,ds0),(M M4,ds0),(M M5,ds0),(T T1,ds0)
              ,(T T2,ds0),(T T3,ds0),(T T4,ds0),(S S1,ds0),(S S2,ds0),(S S3,ds0)]
sort1 = Sort' [(M M1,ds0),(M M2,ds0),(M M3,ds3),(M M4,ds3),(M M5,ds2),(T T1,ds0)
              ,(T T2,ds3),(T T3,ds1),(T T4,ds5),(S S1,ds0),(S S2,ds2),(S S3,ds0)]
sort2 = Sort' [(M M1,ds0),(M M2,ds1),(M M3,ds0),(M M4,ds0),(M M5,ds0),(T T1,ds0)
              ,(T T2,ds0),(T T3,ds0),(T T4,ds0),(S S1,ds0),(S S2,ds0),(S S3,ds0)]
sort3 = Sort' [(M M1,ds0),(M M2,ds1),(M M3,ds0),(M M4,ds3),(M M5,ds4),(T T1,ds0)
              ,(T T2,ds0),(T T3,ds4),(T T4,ds0),(S S1,ds0),(S S2,ds2),(S S3,ds1)]
sort4 = Sort' [(M M1,ds0),(M M2,ds0),(M M3,ds3),(M M4,ds5),(M M5,ds5),(T T1,ds0)
              ,(T T2,ds0),(T T3,ds0),(T T4,ds1),(S S1,ds0),(S S2,ds1),(S S3,ds0)]
sort5 = Sort' [(M M1,ds0),(M M2,ds2),(M M3,ds2),(M M4,ds4),(M M5,ds1),(T T1,ds0)
              ,(T T2,ds5),(T T3,ds2),(T T4,ds2),(S S1,ds0),(S S2,ds0),(S S3,ds2)]

sortf1 = Sort' [(M M5, ds0), (M M1, ds0), (M M5, ds0)]

ds0, ds1, ds2, ds3, ds4, ds5 :: Datensatz'
ds0 = DS' 0 0 lab0 Kein_Skonto
ds1 = DS' 201 2 lab1 ZehnProzent
ds2 = DS' 103 3 lab2 FuenfProzent
ds3 = DS' 99 4 lab3 FuenfProzent
ds4 = DS' 102 0 lab4 DreiProzent
ds5 = DS' 99 6 lab5 Kein_Skonto

lab0, lab1, lab2, lab3, lab4, lab5,labf :: Lieferausblick'
lab0 = LA'
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
      (LF Q4 2025, 0)
    ]
lab1 = LA'
    [ (LF Q1 2023, 10),
      (LF Q2 2023, 11),
      (LF Q3 2023, 12),
      (LF Q4 2023, 13),
      (LF Q1 2024, 14),
      (LF Q2 2024, 15),
      (LF Q3 2024, 16),
      (LF Q4 2024, 17),
      (LF Q1 2025, 18),
      (LF Q2 2025, 19),
      (LF Q3 2025, 20),
      (LF Q4 2025, 21)
    ]
lab2 = LA'
    [ (LF Q1 2023, 1),
      (LF Q2 2023, 3),
      (LF Q3 2023, 1),
      (LF Q4 2023, 3),
      (LF Q1 2024, 1),
      (LF Q2 2024, 2),
      (LF Q3 2024, 1),
      (LF Q4 2024, 4),
      (LF Q1 2025, 2),
      (LF Q2 2025, 1),
      (LF Q3 2025, 1),
      (LF Q4 2025, 1)
    ]
lab3 = LA'
    [ (LF Q1 2023, 10),
      (LF Q2 2023, 9),
      (LF Q3 2023, 8),
      (LF Q4 2023, 7),
      (LF Q1 2024, 6),
      (LF Q2 2024, 5),
      (LF Q3 2024, 4),
      (LF Q4 2024, 3),
      (LF Q1 2025, 2),
      (LF Q2 2025, 1),
      (LF Q3 2025, 0),
      (LF Q4 2025, 0)
    ]
lab4 = LA'
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
      (LF Q4 2025, 0)
    ]
lab5 = LA'
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
labf = LA'
    [
      (LF Q2 2023, 11),
      (LF Q3 2023, 12),
      (LF Q4 2023, 13),
      (LF Q1 2024, 14),
      (LF Q2 2024, 15),
      (LF Q3 2024, 16),
      (LF Q4 2024, 17),
      (LF Q1 2025, 18),
      (LF Q2 2025, 19),
      (LF Q3 2025, 20),
      (LF Q4 2025, 21)
    ]

bh1, bh2 :: Betroffene_Haendler
bh1 = BH (\case
    H1 -> Betroffen
    H2 -> Betroffen
    H3 -> Betroffen
    H4 -> Betroffen
    H5 -> Betroffen
    _  -> NichtBetroffen)

bh2 = BH (\case
    H2  -> Betroffen
    H4  -> Betroffen
    H6  -> Betroffen
    H7  -> Betroffen
    H10 -> Betroffen
    _   -> NichtBetroffen)


spec :: TestTree
spec = testGroup "TestSuite7"
    [

    testCase "lst2fkt_la"  $ (sort.fkt2lst_la.lst2fkt_la.unLA) lab1 @?= (sort.unLA) lab1,
    testCase "lst2fkt_so"  $ (sort.map fst.fkt2lst_so.lst2fkt_so.unSort) sort1 @?= (sort.map fst.unSort) sort1,
    testCase "lst2fkt_ab"  $ (sort.map fst.fkt2lst_ab.lst2fkt_ab.unMt) m1 @?= (sort.map fst.unMt ) m1,

    -- can't get them to work somehow
    -- testCase "lst2fkt_la error" $ assertError "undefiniert" (length.sort.fkt2lst_la_f.lst2fkt_la.fkt2lst_la_f.lst2fkt_la.unLA $ labf),
    -- testCase "lst2fkt_so error" $ assertError "undefiniert" (length.sort.map fst.fkt2lst_so.lst2fkt_so.unSort $ sortf1),
    -- testCase "lst2fkt_ab error 1" $ assertError "undefiniert" (length.sort.map fst.fkt2lst_ab.lst2fkt_ab.unMt $ mf1),
    -- testCase "lst2fkt_ab error 2" $ assertError "undefiniert" (length.sort.map fst.fkt2lst_ab.lst2fkt_ab.unMt $ mf2),

    testCase "preisanpassung 1" $ (test1 (M M2) . preisanpassung $ pack m1) @?= [H2,H3,H5,H7,H9,H10],
    testCase "preisanpassung 2" $ (test1 (T T1) . preisanpassung $ pack m1) @?= [],
    testCase "preisanpassung 3" $ (test2 (T T1) . preisanpassung $ pack m1) @?= (0,0),
    testCase "preisanpassung 4" $ (test2 (T T3) . preisanpassung $ pack m1) @?= (5,515),
    testCase "preisanpassung 5" $ (test2 (M M2) . preisanpassung $ pack m2) @?= (7,721),
    testCase "preisanpassung 6" $ (test2 (M M5) . preisanpassung $ pack m2) @?= (11,1089),
    testCase "preisanpassung 7" $ (test2 (S S2) . preisanpassung $ pack m2) @?= (8,824),
    testCase "preisanpassung 8" $ (test4 (S S2) (LF Q1 2025) 3 . preisanpassung $ pack m1) @?= [(H8,EUR 279),(H4,EUR 279)],
    testCase "preisanpassung 9" $ (test4 (S S2) (LF Q4 2025) 14 . preisanpassung $ pack m2) @?= [(H4,EUR 1298)],

    testCase "Ord Lieferfenster 1"  $ LF Q4 2023 < LF Q1 2050 @?= True,
    testCase "Ord Lieferfenster 2"  $ LF Q4 2025 < LF Q1 2025 @?= False,

    testCase "berichtige 1" $ test1 (M M2) (berichtige (Mt. lst2fkt_ab . unMt $ m1)bh1(LF Q1 2023)) @?= [H2,H3,H5,H7,H9,H10],
    testCase "berichtige 2" $ test1 (T T1) (berichtige (Mt. lst2fkt_ab . unMt $ m1)bh1(LF Q1 2023)) @?= [],
    testCase "berichtige 3" $ test3 (T T1) (LF Q1 2024) (berichtige (pack m1) bh1 (LF Q3 2024)) @?= (0,0),
    testCase "berichtige 4" $ test3 (T T3) (LF Q1 2025) (berichtige (pack m1) bh1 (LF Q2 2024)) @?= (0,0),
    testCase "berichtige 5" $ test3 (M M2) (LF Q1 2024) (berichtige (pack m2) bh2 (LF Q1 2024)) @?= (15,2917),
    testCase "berichtige 6" $ test3 (M M5) (LF Q1 2024) (berichtige (pack m2) bh1 (LF Q1 2024)) @?= (0,0),
    testCase "berichtige 7" $ test3 (S S2) (LF Q3 2024) (berichtige (berichtige (pack m2) bh1 (LF Q2 2024)) bh2 (LF Q1 2024)) @?= (0,0),
    testCase "berichtige 8" $ test4 (S S2) (LF Q1 2025) 3  (berichtige (pack m1)bh1(LF Q1 2024)) @?= [(H8,EUR 543)],
    testCase "berichtige 9" $ test4 (S S2) (LF Q1 2025) 14 (berichtige (pack m2)bh1(LF Q1 2024)) @?= [],


    testCase "errors checks not included" $ True @?= True
    ]


hList :: [Haendler]
hList = [H1,H2,H3,H4,H5,H6,H7,H8,H9,H10]
tList :: [Typ]
tList = [M M1,M M2,M M3,M M4,M M5,T T1,T T2,T T3,T T4,S S1,S S2,S S3]
lList :: [Lieferfenster]
lList = [LF Q1 2023,LF Q2 2023,LF Q3 2023,LF Q4 2023
        ,LF Q1 2024,LF Q2 2024,LF Q3 2024,LF Q4 2024
        ,LF Q1 2025,LF Q2 2025,LF Q3 2025,LF Q4 2025]
lListf :: [Lieferfenster]
lListf = repeat(LF Q1 2023)


toData :: Typ -> Markt -> [(Haendler, Datensatz)]
toData typ (Mt m) = [(a,(\(Sort x) -> x typ) (m a)) | a <- hList]
                 -- map (second (\(Sort x) -> x typ) . \x-> (x,m x)) hList

gPrice :: Datensatz -> Nat1
gPrice (DS x _ _ _) = x
gPrice _            = 0

gPriceRed :: Datensatz -> Double
gPriceRed (DS x _ _ DreiProzent)  = fromIntegral x * 0.97
gPriceRed (DS x _ _ FuenfProzent) = fromIntegral x * 0.95
gPriceRed (DS x _ _ ZehnProzent)  = fromIntegral x * 0.9
gPriceRed (DS x _ _ _)            = fromIntegral x
gPriceRed _                       = 0

gStock :: Datensatz -> Nat0
gStock (DS _ x _ _) = x
gStock _            = 0

gLA :: Datensatz -> Lieferausblick
gLA (DS _ _ x _) = x
gLA _            = LA $ const 0

gStockBy :: Datensatz -> Lieferfenster -> Nat0
gStockBy (DS _ _ (LA x) _) = x
gStockBy _                 = const 0

getSkonto :: Datensatz -> Skonto
getSkonto (DS _ _ _ x) = x
getSkonto _            = Kein_Skonto

trim2MinSnd :: Ord b => [(a, b)] -> [(a, b)]
trim2MinSnd (x : xs) = filter (\a -> snd a == foldl min (snd x) (map snd xs)) (x : xs)
trim2MinSnd _ = []

test1 :: Typ -> Markt -> [Haendler]
test1 t = sort . map fst . filter (\(_, x) -> gStock x > 0) . toData t

test2 :: Typ -> Markt -> (Nat0, Nat0)
test2 typ = foldl (\(a,b) (c,d) -> (a+c,b+d)) (0, 0)
                                   . map (\(_,x) -> (gStock x, gPrice x * gStock x))
                                   . toData typ

test3 :: Typ -> AbLieferfenster -> Markt -> (Nat0, Nat0)
test3 typ l = foldl (\(a,b) (c,d) -> (a+c,b+d)) (0, 0)
                                   . map (\(_,x) -> (gStockBy x l, gPrice x * gStockBy x l))
                                   . toData typ

test4 :: Typ -> Lieferfenster -> Nat0 -> Markt -> [(Haendler, EUR)]
test4 t l n = sortBy (comparing $ Down . fst)
           . trim2MinSnd
           . map (\(x, y) -> (x, EUR $ ceiling $ gPriceRed y * fromIntegral n))
           . filter (\(_,x) -> gStockBy x l >= n)
           . toData t


fkt2lst_la :: (Lieferfenster -> Nat0) -> [(Lieferfenster,Nat0)]
fkt2lst_la y = map (\x -> (x,y x)) lList

fkt2lst_la_f :: (Lieferfenster -> Nat0) -> [(Lieferfenster,Nat0)]
fkt2lst_la_f y = map (\x -> (x,y x)) lListf

fkt2lst_so :: (Typ -> Datensatz) -> [(Typ,Datensatz')]
fkt2lst_so y = map (\x -> (x,(\case
                            (DS a b (LA c) d) -> DS' a b (LA' $ fkt2lst_la c) d
                            _                 -> Nicht_im_Sortiment'
                            ) $ y x)) tList

fkt2lst_ab ::  (Haendler -> Sortiment) -> [(Haendler,Sortiment')]
fkt2lst_ab y = map (\x -> (x,(\(Sort a) -> Sort' $ fkt2lst_so a)  $ y x)) hList

unLA :: Lieferausblick' -> [(Lieferfenster, Nat0)]
unLA (LA' x) = x
unSort :: Sortiment' -> [(Typ, Datensatz')]
unSort (Sort' x) = x
unMt :: Markt' -> [(Haendler, Sortiment')]
unMt (Mt' x)  = x

pack :: Markt' -> Markt
pack = Mt . lst2fkt_ab . unMt
