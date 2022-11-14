{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Angabe5 where
import Data.Maybe

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

newtype EUR  = EUR { euro :: Nat1 }

data Skonto  = Kein_Skonto
               | DreiProzent  
               | FuenfProzent
               | ZehnProzent

data Waschmaschine    = M1 | M2 | M3 | M4 | M5
data Waeschetrockner  = T1 | T2 | T3 | T4
data Waescheschleuder = S1 | S2 | S3

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr
                        }


data Datensatz
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        }
     | Nicht_im_Sortiment

newtype Sortiment = Sort [(Typ,Datensatz)]
newtype Lieferausblick = LA [(Lieferfenster,Nat0)]
newtype Anbieter = A [(Haendler,Sortiment)]

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10


type Suchanfrage = Typ  

----------------- helper functions ----------------

toData :: Typ -> Anbieter -> [(Haendler, Datensatz)]
toData typ (A a)= mapMaybe (\(x,Sort y) -> (\(v,w) -> if isNothing w then Nothing else Just (v,fromJust w))( x,(\v-> if null a then Nothing else Just (head v)) . map snd $ filter (\g -> fst g == typ) y )) a


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
   | otherwise = (\case []-> 0
                        a  -> (snd $ head a)) $ filter (\a->fst a == lff) x
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

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates (x:xs) = elem x xs || hasDuplicates xs
hasDuplicates _      = False














class Wgf a where                -- Wgf fuer `wohlgeformt'
 ist_wgf    :: a -> Bool         -- ist_wgf fuer `ist wohlgeformt'
 ist_nwgf   :: a -> Bool         -- ist_nwgf fuer `ist nicht wohlgeformt'
 wgf_fehler :: a -> b
 -- Protoimplementierungen
 ist_wgf x  = not (ist_nwgf x)
 ist_nwgf x = not (ist_wgf x)
 wgf_fehler = \x -> error "Argument fehlerhaft"


-- Aufgabe A.1

instance Wgf Lieferausblick where
   ist_wgf :: Lieferausblick -> Bool
   ist_wgf (LA (x:xs)) = subCheck x xs && ist_wgf xs
      where
         subCheck :: (Lieferfenster, Nat0) -> [(Lieferfenster, Nat0)] -> Bool
         subCheck (x1,x2) ((y1,y2):ys)
            | x1 == y1 = x2==y2 && subCheck (x1,x2) ys
            | otherwise = subCheck (x1,x2) ys
         subCheck _ _ = True
   ist_wgf _ = True

   ist_nwgf :: Lieferausblick -> Bool
   ist_nwgf = not . ist_wgf

   wgf_fehler :: Lieferausblick -> b
   wgf_fehler = error "Ausblickfehler"

instance Wgf Sortiment where
 ist_wgf = error "Noch nicht implementiert!"

instance Wgf Anbieter where
   ist_wgf :: Anbieter -> Bool
   ist_wgf  = not . ist_nwgf

   ist_nwgf :: Anbieter -> Bool
   ist_nwgf (A x) = hasDuplicates (map fst x) ||  any (ist_nwgf . snd) x

   wgf_fehler :: Anbieter -> b
   wgf_fehler = error "Anbieterfehler"






{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer Wgf folgendermassen vor:
   ...
-}


-- Aufgabe A.5

type Haendlerliste = [Haendler]

sofort_lieferfaehig :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.6

type Stueckzahl  = Nat0
type Gesamtpreis = Nat0
 
sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
sofort_erhaeltliche_Stueckzahl _ a _ = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.7

type Preis = EUR
guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
guenstigste_Lieferanten = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.8

type RabattierterPreis = EUR

guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler,RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}

