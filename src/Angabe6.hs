{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use sortOn" #-}
module Angabe6 where

import           Data.Bifunctor
import           Data.List
import           Data.Maybe
import           Data.Ord

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}

type Nat0 = Int -- Natürliche Zahlen beginnend mit 0

type Nat1 = Int -- Natürliche Zahlen beginnend mit 1

newtype EUR = EUR {euro :: Nat1}
  deriving (Eq, Ord, Show)

data Skonto
  = Kein_Skonto
  | DreiProzent
  | FuenfProzent
  | ZehnProzent
  deriving (Eq, Ord, Show, Enum, Bounded)

data Waschmaschine = M1 | M2 | M3 | M4 | M5
  deriving (Eq, Ord, Show, Enum, Bounded)

data Waeschetrockner = T1 | T2 | T3 | T4
  deriving (Eq, Ord, Show, Enum, Bounded)

data Waescheschleuder = S1 | S2 | S3
  deriving (Eq, Ord, Show, Enum, Bounded)

data Typ
  = M Waschmaschine
  | T Waeschetrockner
  | S Waescheschleuder
  deriving (Eq, Show, Ord)

instance Enum Typ where
  toEnum :: Int -> Typ
  toEnum x
    | x < limit1 = M (toEnum x :: Waschmaschine)
    | x < limit2 = T ((toEnum $ x - limit1) :: Waeschetrockner)
    | otherwise = S ((toEnum $ x - limit2) :: Waescheschleuder)
    where
      limit1 = 1 + fromEnum (maxBound :: Waschmaschine)
      limit2 = 1 + fromEnum (maxBound :: Waeschetrockner) + limit1
  fromEnum :: Typ -> Int
  fromEnum (M x) = fromEnum x
  fromEnum (T x) = fromEnum x + fromEnum (maxBound :: Waschmaschine) + 1
  fromEnum (S x) = fromEnum x + fromEnum (maxBound :: Waschmaschine) + fromEnum (maxBound :: Waeschetrockner) + 2

instance Bounded Typ where
  minBound :: Typ
  minBound = M minBound
  maxBound :: Typ
  maxBound = S maxBound

data Quartal = Q1 | Q2 | Q3 | Q4
  deriving (Eq, Ord, Show, Enum, Bounded)

data Jahr = J2023 | J2024 | J2025
  deriving (Eq, Ord, Show, Enum, Bounded)

data Lieferfenster = LF
  { quartal :: Quartal,
    jahr    :: Jahr
  }
  deriving (Eq, Show, Bounded)

instance Ord Lieferfenster where
  compare :: Lieferfenster -> Lieferfenster -> Ordering
  compare (LF a1 a2) (LF b1 b2)
    | x == EQ = compare a1 b1
    | otherwise = x
    where
      x = compare a2 b2

instance Enum Lieferfenster where
  toEnum :: Int -> Lieferfenster
  toEnum n = LF (toEnum (mod n 4)) (toEnum (div n 4))

  fromEnum :: Lieferfenster -> Int
  fromEnum (LF q y) = fromEnum y * 4 + fromEnum q

data Datensatz
  = DS
      { preis_in_euro                        :: Nat1,
        sofort_lieferbare_stueckzahl         :: Nat0,
        lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
        skonto                               :: Skonto
      }
  | Nicht_im_Sortiment

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10
  deriving (Eq, Ord, Show, Enum, Bounded)

lList :: [Lieferfenster]
lList = [minBound .. maxBound]

tList :: [Typ]
tList = [minBound .. maxBound]

hList :: [Haendler]
hList = [minBound .. maxBound]

newtype Lieferausblick = LA (Lieferfenster -> Nat0)

newtype Sortiment = Sort (Typ -> Datensatz)

newtype Anbieter = A (Haendler -> Sortiment)

type Suchanfrage = Typ

-- Aufgabe A.1

wg_la :: Lieferausblick -> [(Lieferfenster, Nat0)]
wg_la (LA f) = filter ((/=0) . snd) [(x,f x) |x <- lList]

wg_so :: Sortiment -> [(Typ, Datensatz)]
wg_so (Sort f) = [(x,f x) |x <- tList]

wg_ab :: Anbieter -> [(Haendler, Sortiment)]
wg_ab (A f) = [(x,f x) |x <- hList]

-- helper functions

toData :: Typ -> Anbieter -> [(Haendler, Datensatz)]
toData typ = mapMaybe ((\(x, y) -> maybefy (x, lookup typ y)) . second wg_so) . wg_ab
  where
    maybefy (x, Just y) = Just (x, y)
    maybefy _           = Nothing

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

gStockBy :: Datensatz -> Lieferfenster -> Stueckzahl
gStockBy (DS _ _ (LA x) _) = x
gStockBy _                 = const 0

getSkonto :: Datensatz -> Skonto
getSkonto (DS _ _ _ x) = x
getSkonto _            = Kein_Skonto

trim2MinSnd :: Ord b => [(a, b)] -> [(a, b)]
trim2MinSnd (x : xs) = filter (\a -> snd a == foldl min (snd x) (map snd xs)) (x : xs)
trim2MinSnd _ = []

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates (x : xs) = elem x xs || hasDuplicates xs
hasDuplicates _        = False




-- Aufgabe A.2
class Wgf a where -- Wgf fuer `wohlgeformt'
  ist_wgf :: a -> Bool -- ist_wgf fuer `ist wohlgeformt'
  ist_nwgf :: a -> Bool -- ist_nwgf fuer `ist nicht wohlgeformt'
  wgf_fehler :: a -> a
  wgf_check :: a -> a

  -- Protoimplementierungen
  ist_wgf = const True
  ist_nwgf = not . ist_wgf
  wgf_check x = if ist_nwgf x then wgf_fehler x else x
  wgf_fehler = error "Argument fehlerhaft"

instance Wgf Lieferausblick where
  wgf_fehler :: Lieferausblick -> Lieferausblick
  wgf_fehler = error "Ausblickfehler"

  -- ist_wgf :: Lieferausblick -> Bool
  -- ist_wgf a = mainCheck $ wg_la a
  --   where
  --     mainCheck :: [(Lieferfenster, Nat0)] -> Bool
  --     mainCheck (x : xs) = subCheck x xs && mainCheck xs
  --     mainCheck _ = True    

  --     subCheck :: (Lieferfenster, Nat0) -> [(Lieferfenster, Nat0)] -> Bool
  --     subCheck (x1, x2) ((y1, y2) : ys)
  --           | x1 == y1 = x2 == y2 && subCheck (x1, x2) ys
  --           | otherwise = subCheck (x1, x2) ys
  --     subCheck _ _ = True


instance Wgf Sortiment where
  wgf_fehler :: Sortiment -> Sortiment
  wgf_fehler = error "Sortimentfehler"

instance Wgf Anbieter where
  wgf_fehler :: Anbieter -> Anbieter
  wgf_fehler = error "Anbieterfehler"

-- Aufgabe A.5

type Haendlerliste = [Haendler]

sofort_lieferfaehig :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig typ = sortBy (comparing Down)
                        . map fst
                        . filter (\(_,x) -> gStock x > 0)
                        . toData typ

-- Aufgabe A.6

type Stueckzahl = Nat0

type Gesamtpreis = Nat0

sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl, Gesamtpreis)
sofort_erhaeltliche_Stueckzahl typ = foldl (\(a,b) (c,d) -> (a+c,b+d)) (0, 0)
                                   . map (\(_,x) -> (gStock x, gPrice x * gStock x))
                                   . toData typ

-- Aufgabe A.7

type Preis = EUR

guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
guenstigste_Lieferanten typ lff = (\x -> if null x then Nothing else Just x)
                                . sortBy (comparing Down)
                                . map fst
                                . trim2MinSnd
                                . map (second gPrice)
                                . filter (\x -> gStockBy (snd x) lff > 0)
                                . toData typ


-- Aufgabe A.8

type RabattierterPreis = EUR

guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler, RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster typ lff n = sortBy (comparing $ Down . fst)
                                                   . trim2MinSnd
                                                   . map (\(x, y) -> (x, EUR . (\a -> 10 * ceiling (a / 10)) $ gPriceRed y * fromIntegral n))
                                                   . filter (\x -> gStockBy (snd x) lff >= n)
                                                   . toData typ

