module Angabe6 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1

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
data Jahr          = J2023 | J2024 | J2025
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr 
                        }

newtype Lieferausblick = LA (Lieferfenster -> Nat0)

data Datensatz 
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        }
     | Nicht_im_Sortiment

newtype Sortiment = Sort (Typ -> Datensatz)

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10

newtype Anbieter = A (Haendler -> Sortiment)

type Suchanfrage = Typ  

class Wgf a where                -- Wgf fuer `wohlgeformt'
 ist_wgf    :: a -> Bool         -- ist_wgf fuer `ist wohlgeformt'
 ist_nwgf   :: a -> Bool         -- ist_nwgf fuer `ist nicht wohlgeformt'
 wgf_fehler :: a -> a
 -- Protoimplementierungen
 ist_wgf x  = not (ist_nwgf x)
 ist_nwgf x = not (ist_wgf x)
 wgf_fehler = \x -> error "Argument fehlerhaft"


-- Aufgabe A.1

wg_la :: Lieferausblick -> [(Lieferfenster,Nat0)]

wg_so :: Sortiment -> [(Typ,Datensatz)]

wg_ab :: Anbieter ->  [(Haendler,Sortiment)


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

