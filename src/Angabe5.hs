{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# HLINT ignore "Use infix" #-}
module Angabe5 where
import           Data.Bifunctor
import           Data.Maybe

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

newtype EUR  = EUR { euro :: Nat1 }
   deriving (Eq,Ord,Show)

data Skonto  = Kein_Skonto
               | DreiProzent
               | FuenfProzent
               | ZehnProzent
               deriving (Eq, Ord, Show, Enum, Bounded)

data Waschmaschine    = M1 | M2 | M3 | M4 | M5
   deriving (Eq, Enum, Bounded, Show)
data Waeschetrockner  = T1 | T2 | T3 | T4
   deriving (Eq, Enum, Bounded, Show)
data Waescheschleuder = S1 | S2 | S3
   deriving (Eq, Enum, Bounded, Show)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder
           deriving (Eq, Show)

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr
                        } deriving (Eq, Ord, Show)

type Lieferausblick = [(Lieferfenster,Nat0)]

data Datensatz
   = DS { preis_in_euro                        :: Nat1,
          sofort_lieferbare_stueckzahl         :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto                               :: Skonto
        }
     | Nicht_im_Sortiment
     deriving (Eq,Show)

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10
   deriving (Eq, Enum, Bounded, Show)

type Anbieter = [(Haendler,Sortiment)]

type Sortiment = [(Typ,Datensatz)]

type Suchanfrage = Typ

class Wgf a where                  -- Wgf fuer `wohlgeformt'
   ist_wgf    :: a -> Bool         -- ist_wgf fuer `ist wohlgeformt'
   ist_nwgf   :: a -> Bool         -- ist_nwgf fuer `ist nicht wohlgeformt'
   wgf_fehler :: a -> b
   -- Protoimplementierungen
   ist_wgf x  = not (ist_nwgf x)
   ist_nwgf x = not (ist_wgf x)
   wgf_fehler = error "Argument fehlerhaft"








-- global helper function


hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates (x:xs) = elem x xs || hasDuplicates xs
hasDuplicates _      = False

-- made them thinking they make sense, never used them
matchL :: Eq a => a -> [(a,b)] -> Maybe b
matchL a b
   | length bs > 1 = error "uncaught error, fix me above matchL"
   | null bs = Nothing
   | otherwise = Just $ snd $ head bs
   where
      bs = filter (\x -> a == fst x) b

matchR :: Eq a => a -> [(b,a)] -> Maybe b
matchR a b
   | length bs > 1 = error "uncaught error, fix me above matchR"
   | null bs = Nothing
   | otherwise = Just $ fst $ head bs
   where
      bs = filter (\x -> a == snd x) b


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

gStockBy :: Datensatz -> Lieferfenster -> Stueckzahl
gStockBy (DS _ _ x _) lff
   | ist_nwgf x = wgf_fehler x
   | otherwise =snd . head $ filter (\a->fst a == lff) x
gStockBy _ _              = 0

getSkonto :: Datensatz -> Skonto
getSkonto (DS _ _ _ x) =  x
getSkonto _            = Kein_Skonto

trim2MinSnd :: Ord b => [(a, b)] -> [(a, b)]
trim2MinSnd trimList
  | isNothing(minSnd trimList) = []
  | otherwise = let minVal = fromJust (minSnd trimList)
                in filter (\y -> snd y == minVal) trimList
  where
  minSnd :: Ord b => [(a, b)] -> Maybe b
  minSnd ((_,x):xs) = Just $ foldl min x (map snd xs)
  minSnd _          = Nothing


toData :: Typ -> Anbieter -> [(Haendler, Datensatz)]
toData typ = map (\(x,y) -> ( x,head . map snd $ filter (\g -> fst g == typ) y ) )
-- toData can only return a single Datensatz per Typ and Haendler
-- luckily that's covered by Wgf




-- Aufgabe A.1

instance Wgf  [(Typ, Datensatz)] where

   ist_wgf :: [(Typ, Datensatz)] -> Bool
   ist_wgf x  = not (ist_nwgf x)

   ist_nwgf :: [(Typ, Datensatz)] -> Bool
   ist_nwgf x = hasDuplicates $ map fst x

   wgf_fehler :: [(Typ, Datensatz)] -> b
   wgf_fehler = error "Sortimentfehler"




instance Wgf  [(Haendler, Sortiment)] where

   ist_wgf :: [(Haendler, Sortiment)] -> Bool
   ist_wgf x  = not (ist_nwgf x)

   ist_nwgf :: [(Haendler, Sortiment)] -> Bool
   ist_nwgf x = hasDuplicates (map fst x) ||  any (ist_nwgf . snd) x

   wgf_fehler :: [(Haendler, Sortiment)] -> b
   wgf_fehler = error "Anbieterfehler"


instance Wgf  [(Lieferfenster, Nat0)] where

   ist_wgf :: [(Lieferfenster, Nat0)] -> Bool
   ist_wgf (x:xs) = subCheck x xs && ist_wgf xs
      where
         subCheck :: (Lieferfenster, Nat0) -> [(Lieferfenster, Nat0)] -> Bool
         subCheck (x1,x2) ((y1,y2):ys)
            | x1 == y1 = x2==y2 && subCheck (x1,x2) ys
            | otherwise = subCheck (x1,x2) ys
         subCheck _ _ = True
   ist_wgf _ = True

   ist_nwgf :: [(Lieferfenster, Nat0)] -> Bool
   ist_nwgf x = not (ist_wgf x)

   wgf_fehler :: [(Lieferfenster, Nat0)] -> b
   wgf_fehler = error "Ausblickfehler"



{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer Wgf folgendermassen vor:
   ...
-}


-- Aufgabe A.5

type Haendlerliste = [Haendler]

sofort_lieferfaehig :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig typ a
   | ist_nwgf a = wgf_fehler a
   | otherwise = map fst . filter (\(_,x) -> gStock x > 0) $ toData typ a


--        map fst $ filter (hasIt typ . snd) a
--    where

--       hasIt :: Typ -> Sortiment -> Bool
--       hasIt typ ((t,d):xs)
--          | t == typ = gStock d > 0
--          | otherwise = hasIt typ xs
--       hasIt _ _ = False

-- addTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
-- addTuple (a,b) (c,d) = (a+c,b+d)

{- Knapp, aber gut nachvollziehbar geht die Implementierung olgendermassen vor:
   ...
-}


-- Aufgabe A.6

type Stueckzahl  = Nat0
type Gesamtpreis = Nat0
sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
sofort_erhaeltliche_Stueckzahl typ anbieter
   | ist_nwgf anbieter = wgf_fehler anbieter
   | otherwise = foldl (\(a,b)(c,d)->(a+c,b+d))(0,0) . map(\(_,x)-> (gPrice x,gPrice x *  gStock x)) $ toData typ anbieter



-- sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
-- sofort_erhaeltliche_Stueckzahl typ a
--    | ist_nwgf a = wgf_fehler a
--    | otherwise = buildPriceList typ a
--    where
--       buildPriceList :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
--       buildPriceList typ ((_,x):xs) = addTuple (findPriceList typ x) (buildPriceList typ xs)
--       buildPriceList _ _ = (0, 0)

--       findPriceList :: Typ -> Sortiment -> (Stueckzahl,Gesamtpreis)
--       findPriceList typ ((t,d):xs)
--          | t == typ = (gStock d, gStock d * gPrice d)
--          | otherwise = findPriceList typ xs
--       findPriceList _ _ = (0,0)


{- Knapp, aber gut nachvollziehbar geht die Implementierung olgendermassen vor:
   ...
-}


-- Aufgabe A.7
 -- TODO test if nwgf_error Lieferaussicht works form helper
type Preis = EUR
guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
guenstigste_Lieferanten typ lff a
   | ist_nwgf a = wgf_fehler a
   | otherwise  = (\x -> if null x then Nothing else Just x) .
                  map fst . trim2MinSnd . map (second gPrice) $
                  filter (\x -> gStockBy (snd x) lff> 0)$
                  toData typ a

{- Knapp, aber gut nachvollziehbar geht die Implementierung olgendermassen vor:
   ...
-}


-- Aufgabe A.8

type RabattierterPreis = EUR
 -- TODO test if nwgf_error Lieferaussicht works form helper
guenstigste_Lieferanten_im_Lieferfenster ::
   Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter
   -> [(Haendler,RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster typ lff n a
   | ist_nwgf a = wgf_fehler a
   | otherwise  = trim2MinSnd .
                  map (\(x,y) -> (x,EUR $ ceiling (gPriceRed y * fromIntegral n))) .
                  filter (\(_,x) -> gStockBy x lff >= n) $
                  toData typ a


{- Knapp, aber gut nachvollziehbar geht die Implementierung olgendermassen vor:
   ...
-}




anb1,anb2,anbf1,anbf2 :: Anbieter
anb1 = [(H1,sort1),
        (H2,sort2),
        (H3,sort3),
        (H4,sort4),
        (H5,sort5),
        (H6,sort6),
        (H7,sort7),
        (H8,sort8),
        (H9,sort9),
        (H10,sort10)
        ]
anb2 = [(H1,sort1),
        (H2,sort2),
        (H3,sort3)
        ]
anbf1 =[(H1,sort1),
        (H2,sort2),
        (H3,sort3),
        (H4,sort4),
        (H5,sort5),
        (H6,sort6),
        (H6,sort6),--duplicate
        (H7,sort7),
        (H8,sort8),
        (H9,sort9),
        (H10,sort10)
        ]
anbf2 =[(H1,sort1),
        (H2,sort2),
        (H3,sort3),
        (H4,sort4),
        (H5,sort5),
        (H6,sortf1), -- faulty Sortiment
        (H7,sort7),
        (H8,sort8),
        (H9,sort9),
        (H10,sort10)]
sort1, sort2, sort3, sort4, sort5, sort6, sort7, sort8, sort9, sort10, sortf1, sortf2 :: Sortiment
sort1  = [(M M1,ds2),(M M2,ds2),(M M3,ds2),(M M4,ds2),(M M5,ds2),(T T1,ds2),(T T2,ds2),(T T3,ds2),(T T4,ds2),(S S1,ds2),(S S2,ds2),(S S3,ds2)]
sort2  = [(M M1,ds2),(M M2,ds2),(M M3,ds2),(M M4,ds2),(M M5,ds2)]
sort3  = [(T T1,ds2),(T T2,ds2),(T T3,ds2),(T T4,ds2)]
sort4  = [(S S1,ds2),(S S2,ds2),(S S3,ds2)]
sort5  = [(M M1,ds2),(T T1,ds2),(S S1,ds2)]
sort6  = [(M M2,ds2),(T T2,ds2),(S S2,ds2)]
sort7  = [(M M3,ds2),(T T3,ds2),(S S3,ds2)]
sort8  = [(M M4,ds2),(T T4,ds2)]
sort9  = [(M M5,ds2)]
sort10 = [(M M5,ds2),(M M1,ds2),(M M5,ds2),(M M5,ds2)]
sortf1 = [(M M5,ds2),(M M1,ds2),(M M5,ds2)] --2* M M5
sortf2 = [(M M1,ds2),(M M2,ds2),(M M3,ds2),(M M4,ds2),(M M5,ds2),(T T1,ds2),(T T2,ds2),(T T3,ds2),(T T3,ds2),(T T4,ds2),(S S1,ds2),(S S2,ds2),(S S3,ds2)] --2* T t3

ds0, ds1, ds2, ds3, ds4, ds5 :: Datensatz

ds0 = DS 0   0 lab0 Kein_Skonto
ds1 = DS 201 2 lab1 ZehnProzent
ds2 = DS 103 3 lab2 FuenfProzent
ds3 = DS 99  4 lab3 FuenfProzent
ds4 = DS 102 5 lab4 DreiProzent
ds5 = DS 99  6 lab5 Kein_Skonto

lab0, lab1, lab2, lab3, lab4, lab5, lab6, labf :: Lieferausblick
lab0 = []
lab1 = [(LF Q1 2023, 10),
        (LF Q2 2023, 11),
        (LF Q3 2023, 12),
        (LF Q4 2023, 13),
        (LF Q4 2023, 13),--duplicate
        (LF Q1 2024, 14),
        (LF Q2 2024, 15),
        (LF Q3 2024, 16),
        (LF Q4 2024, 17),
        (LF Q1 2025, 18),
        (LF Q2 2025, 19),
        (LF Q3 2025, 20),
        (LF Q4 2025, 21)
        ]
lab2 = [(LF Q1 2023,  1),
        (LF Q2 2023,  3),
        (LF Q3 2023,  1),
        (LF Q4 2023,  3),
        (LF Q1 2024,  1),
        (LF Q2 2024,  2),
        (LF Q3 2024,  1),
        (LF Q4 2024,  4),
        (LF Q1 2025,  2),
        (LF Q1 2025,  2),--duplicate
        (LF Q2 2025,  1),
        (LF Q3 2025,  1),
        (LF Q4 2025,  1)
        ]
lab3 = [(LF Q1 2023, 10),
        (LF Q2 2023,  9),
        (LF Q3 2023,  8),
        (LF Q4 2023,  7),
        (LF Q1 2024,  6),
        (LF Q2 2024,  5),
        (LF Q3 2024,  4),
        (LF Q4 2024,  3),
        (LF Q1 2025,  2),
        (LF Q1 2025,  2),--duplicate
        (LF Q1 2025,  2),--duplicate
        (LF Q2 2025,  1),
        (LF Q3 2025,  0),
        (LF Q4 2025,  0)
        ]
lab4 = [(LF Q1 2023,  0),
        (LF Q2 2023,  0),
        (LF Q3 2023,  0),
        (LF Q4 2023,  0),
        (LF Q1 2024,  0),
        (LF Q2 2024,  0),
        (LF Q3 2024,  0),
        (LF Q4 2024,  0),
        (LF Q1 2025,  0),
        (LF Q2 2025,  0),
        (LF Q3 2025,  0),
        (LF Q4 2025,  0),
        (LF Q1 2023,  0),--duplicate
        (LF Q2 2023,  0),--duplicate
        (LF Q3 2023,  0),--duplicate
        (LF Q4 2023,  0),--duplicate
        (LF Q1 2024,  0),--duplicate
        (LF Q2 2024,  0),--duplicate
        (LF Q3 2024,  0),--duplicate
        (LF Q4 2024,  0),--duplicate
        (LF Q1 2025,  0),--duplicate
        (LF Q2 2025,  0),--duplicate
        (LF Q3 2025,  0),--duplicate
        (LF Q4 2025,  0)--duplicate
        ]
lab5 = [(LF Q3 2024, 0),--shuffled
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
lab6 = [(LF Q3 2024, 0),--shuffled
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
labf = [(LF Q1 2023, 0),
        (LF Q2 2023, 0),
        (LF Q3 2023, 0),
        (LF Q4 2023, 0),
        (LF Q4 2023, 1),--duplicate, differing
        (LF Q1 2024, 0),
        (LF Q2 2024, 0),
        (LF Q3 2024, 0),
        (LF Q4 2024, 0),
        (LF Q1 2025, 0),
        (LF Q2 2025, 0),
        (LF Q3 2025, 0),
        (LF Q4 2025, 0)]

