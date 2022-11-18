{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}
module Angabe6 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1

newtype EUR  = EUR { euro :: Nat1 }
   deriving (Eq,Ord,Show)

data Skonto  = Kein_Skonto
               | DreiProzent
               | FuenfProzent
               | ZehnProzent
   deriving (Eq,Ord,Show,Enum,Bounded)

data Waschmaschine    = M1 | M2 | M3 | M4 | M5
   deriving (Eq,Ord,Show,Enum,Bounded)

data Waeschetrockner  = T1 | T2 | T3 | T4
   deriving (Eq,Ord,Show,Enum,Bounded)

data Waescheschleuder = S1 | S2 | S3
   deriving (Eq,Ord,Show,Enum,Bounded)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder
   deriving (Eq, Show, Ord)

instance Enum Typ where
  toEnum :: Int -> Typ
  toEnum x
      | x <= maxM = M (toEnum x::Waschmaschine)
      | x <= maxM + maxT +1 = T ((toEnum $ x - maxM-1)::Waeschetrockner)
      | otherwise = S ((toEnum $ x - maxT - maxM -2)::Waescheschleuder)
      where
         maxM = fromEnum (maxBound::Waschmaschine)
         maxT = fromEnum (maxBound::Waeschetrockner)
  fromEnum :: Typ -> Int
  fromEnum (M x) = fromEnum x
  fromEnum (T x) = fromEnum x + fromEnum (maxBound::Waschmaschine) +1
  fromEnum (S x) = fromEnum x + fromEnum (maxBound::Waschmaschine) + fromEnum (maxBound::Waeschetrockner) +2

instance Bounded Typ where
  minBound :: Typ
  minBound = M minBound
  maxBound :: Typ
  maxBound = S maxBound



typList :: [Typ]
typList = [minBound..maxBound] ::[Typ]


data Quartal       = Q1 | Q2 | Q3 | Q4
   deriving (Eq,Ord,Show,Enum,Bounded)

data Jahr          = J2023 | J2024 | J2025
   deriving (Eq,Ord,Show,Enum,Bounded)

data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr
                        }      deriving (Eq,Show,Bounded)

instance Ord Lieferfenster where
   compare :: Lieferfenster -> Lieferfenster -> Ordering
   compare (LF a1 a2) (LF b1 b2)
    | x == EQ = compare a1 b1
    | otherwise = x
    where
      x = compare a2 b2

instance Enum Lieferfenster where

  toEnum :: Int -> Lieferfenster
  toEnum n = LF  (toEnum (mod n 4)) (toEnum (div n 4))

  fromEnum :: Lieferfenster -> Int
  fromEnum (LF q y)= fromEnum y * 4 + fromEnum q

lieferfensterList :: [Lieferfenster]
lieferfensterList = [minBound..maxBound]

data Datensatz
   = DS { preis_in_euro                        :: Nat1,
          sofort_lieferbare_stueckzahl         :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto                               :: Skonto
        }
     | Nicht_im_Sortiment


data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10
   deriving (Eq,Ord,Show,Enum,Bounded)

haendlerList :: [Haendler]
haendlerList = [minBound..maxBound]

newtype Sortiment = Sort (Typ -> Datensatz)
newtype Anbieter = A (Haendler -> Sortiment)
newtype Lieferausblick = LA (Lieferfenster -> Nat0)

type Suchanfrage = Typ

class Wgf a where                -- Wgf fuer `wohlgeformt'
 ist_wgf    :: a -> Bool         -- ist_wgf fuer `ist wohlgeformt'
 ist_nwgf   :: a -> Bool         -- ist_nwgf fuer `ist nicht wohlgeformt'
 wgf_fehler :: a -> a
 -- Protoimplementierungen
 ist_wgf  = not.ist_nwgf
 ist_nwgf = not.ist_wgf
 wgf_fehler = error "Argument fehlerhaft"


-- Aufgabe A.1

-- wg_la :: Lieferausblick -> [(Lieferfenster,Nat0)]

-- wg_so :: Sortiment -> [(Typ,Datensatz)]

-- wg_ab :: Anbieter ->  [(Haendler,Sortiment)]


{- Knapp, aber gut nachvollziehbar gehen die Implementierungen folgendermassen vor:
   ...
-}


-- Aufgabe A.2

instance Wgf Lieferausblick where
 ist_wgf = error "Noch nicht implementiert!"

instance Wgf Sortiment where
 ist_wgf = error "Noch nicht implementiert!"

instance Wgf Anbieter where
  ist_wgf = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer Wgf folgendermassen vor:
   ...
-}


-- Aufgabe A.5

type Haendlerliste = [Haendler]

sofort_lieferfaehig :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig _ _ = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.6

type Stueckzahl  = Nat0
type Gesamtpreis = Nat0

sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
sofort_erhaeltliche_Stueckzahl _ _ = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.7

type Preis = EUR
guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
guenstigste_Lieferanten _ _ = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.8

type RabattierterPreis = EUR

guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler,RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster _ _ = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}

