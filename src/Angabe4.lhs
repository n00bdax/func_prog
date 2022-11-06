> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Use camelCase" #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE DuplicateRecordFields #-}



> module Angabe4 where

1. Vervollständigen Sie gemaess Angabentext!
2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
3. Löschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisung!
4. Achten Sie darauf, dass `Gruppe' Leserechte für Ihre Abgabedatei hat!
5. Hinweis: Kommentar- und Programmzeilen müssen durch mindestens eine Leerzeile getrennt sein!


> type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
> type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
> type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

> newtype EUR  = EUR { euro :: Nat1 }

> data Skonto  = Kein_Skonto 
>                | DreiProzent  
>                | FuenfProzent 
>                | ZehnProzent

> data Waschmaschinentyp   = WM_Typ1 | WM_Typ2 | WM_Typ3 | WM_Typ4 | WM_Typ5
>   deriving (Eq, Bounded,Enum,Show)
> data Waeschetrocknertyp  = WT_Typ1 | WT_Typ2 | WT_Typ3 | WT_Typ4
>   deriving (Eq, Bounded,Enum,Show)
> data Waescheschleudertyp = WS_Typ1 | WS_Typ2 | WS_Typ3
>   deriving (Eq, Bounded,Enum,Show)

> data Typ = WM Waschmaschinentyp
>            | WT Waeschetrocknertyp
>            | WS Waescheschleudertyp

> data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
> type Jahr          = Nat2023
> data Lieferfenster = LF { quartal :: Quartal,
>                           jahr    :: Jahr 
>                         } deriving (Eq, Ord, Show)

> data Datensatz 
>   = DS { preis_in_euro :: Nat1,
>          sofort_lieferbare_stueckzahl :: Nat0,
>          lieferbare_stueckzahl_im_Zeitfenster :: Lieferfenster -> Nat0,
>          skonto :: Skonto
>        }
>     | Nicht_im_Sortiment

> data Sortiment 
>   =   WMS {wm :: Waschmaschinentyp   -> Datensatz}
>     | WTS {wt :: Waeschetrocknertyp  -> Datensatz}
>     | WSS {ws :: Waescheschleudertyp -> Datensatz}

> data Lieferantenname = L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9 | L10
>   deriving (Eq, Bounded, Enum, Show)

> type Lieferantenliste = [Lieferantenname]

> type Lieferanten = Lieferantenname -> Sortiment -> Datensatz

> type Suchanfrage = Typ

----------------------------------------------------------------------------------
                                    utilities
----------------------------------------------------------------------------------

> lfrntn :: Lieferantenliste
> lfrntn = [(L1)..(L10)]

getter functions for Datensatz

> getPrice :: Datensatz -> Nat1
> getPrice (DS {preis_in_euro = x}) = x
> getPrice _ = 0

> getInStock :: Datensatz -> Nat0
> getInStock (DS {sofort_lieferbare_stueckzahl = x}) = x
> getInStock _ = 0

> isInStock :: Datensatz -> Bool
> isInStock (DS {sofort_lieferbare_stueckzahl = 1}) = True
> isInStock _ = False

> getInStockBy :: Datensatz -> (Lieferfenster -> Nat0)
> getInStockBy (DS {lieferbare_stueckzahl_im_Zeitfenster = x}) = x
> getInStockBy _ = neverInStock

> neverInStock :: Lieferfenster -> Nat0
> neverInStock _ = 0

> getSkonto :: Datensatz -> Skonto
> getSkonto (DS {skonto = x}) =x
> getSkonto _ = Kein_Skonto

> dropZeroes :: (Integral a) => [a] -> [a]
> dropZeroes (0:xs) = dropZeroes xs
> dropZeroes (x:xs) = x : dropZeroes xs
> dropZeroes [] = []







avilability patterns for testing 

restocks:
availA  never
availB  Q1
availC  Q3
availD  Q1,Q2,Q3,Q4

> availA, availB, availC, availD :: Lieferfenster -> Nat0
> availA _ = 0
> availB (LF _ y) = y - 2023 
> availC (LF Q3 y) = y - 2023 +1
> availC (LF Q4 y) = y - 2023 +1
> availC (LF _ y) = y - 2023
> availD (LF Q1 y) = (y - 2023)*4
> availD (LF Q2 y) = (y - 2023)*4 +1
> availD (LF Q3 y) = (y - 2023)*4 +2
> availD (LF Q4 y) = (y - 2023)*4 +3

assortments

> assA:: Sortiment
> assA = WMS getAssA

> getAssA :: Waschmaschinentyp -> Datensatz
> getAssA WM_Typ1 = ds1
> getAssA WM_Typ2 = ds2
> getAssA WM_Typ3 = ds3
> getAssA WM_Typ4 = ds4
> getAssA WM_Typ5 = ds5

> ds1, ds2, ds3, ds4, ds5, notAvailable :: Datensatz
> ds1 = DS 1 3 availA Kein_Skonto
> ds2 = DS 2 3 availA Kein_Skonto
> ds3 = DS 3 3 availA Kein_Skonto
> ds4 = DS 4 3 availA Kein_Skonto
> ds5 = DS 5 3 availA Kein_Skonto
> notAvailable = DS 0 0 availA Kein_Skonto

> db1 :: Lieferanten
> db1 L1 assA = ds1
> db1 L2 assA = ds2
> db1 L3 assA = ds3
> db1 _ _ = notAvailable 





        f :: Sortiment -> Waschmaschinentyp -> Datensatz
        f = wm
        has a name (WM typ) = getInStock (a (name wm typ))>0
        has a name (WT typ) = getInStock (a (name wt typ))>0
        has a name (WS typ) = getInStock (a (name ws typ))>0
        has _ _ _ = False

> gg :: Typ -> Sortiment
> gg (WM _) = WMS {wm = nopeM}
> gg (WT _) = WTS {wt = nopeT}
> gg (WS _) = WSS {ws = nopeS}

> nope :: Typ -> Datensatz
> nope _ = notAvailable
> notNope :: Typ -> Datensatz
> notNope _ = ds1

> sortM, sortT, sortS :: Sortiment
> sortM = WMS {wm = nopeM}
> sortT = WTS {wt = nopeT}
> sortS = WSS {ws = nopeS}

> nopeM ::Waschmaschinentyp -> Datensatz
> nopeM _ = notAvailable
> nopeT ::Waeschetrocknertyp -> Datensatz
> nopeT _ = notAvailable
> nopeS ::Waescheschleudertyp -> Datensatz
> nopeS _ = notAvailable

> aa :: Waschmaschinentyp -> Datensatz
> aa WM_Typ1 = nopeM WM_Typ1
> aa _ = notAvailable 

Aufgabe A.1


> sofort_erhaeltlich_bei :: Suchanfrage -> Lieferanten  -> Lieferantenliste
> sofort_erhaeltlich_bei typ a = checkFor a typ lfrntn
>   where
>   checkFor :: Lieferanten -> Typ -> Lieferantenliste -> Lieferantenliste
>   checkFor a typ (x:xs)
>     | has x typ a = x : checkFor a typ xs
>     | otherwise = checkFor a typ xs
>   checkFor _ _ _ = []

>   has :: Lieferantenname -> Typ -> Lieferanten -> Bool
>   has name (WM typ) a = isInStock(a name (wm typ))
>   has _ _ _ = False



 sofort_erhaeltlich_bei= hasInStock lfrntn
   where
   hasInStock :: Lieferantenliste -> Typ -> Lieferanten -> Lieferantenliste
   hasInStock (x:xs)
       | checkStock x = x : hasInStock xs
       | otherwise = hasInStock xs
   hasInStock _= []
   checkStock :: Lieferantenname -> Typ -> Lieferanten -> Bool
   checkStock name (WM typ) x = getInStock2(x name typ)>0 

 
   checkStock :: Lieferantenname -> Typ -> Lieferanten -> Bool




 Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
...


Aufgabe A.2

> type Stueckzahl  = Nat0
> type Gesamtpreis = Nat0

> sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Lieferanten -> (Stueckzahl,Gesamtpreis)
> sofort_erhaeltliche_Stueckzahl = error "noch nicht implementiert"
        
Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
...
 

Aufgabe A.3

> type Preis = EUR
> guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Lieferanten -> Maybe Lieferantenliste
> guenstigste_Lieferanten = error "noch nicht implementiert"

Knapp, aber gut nachvollziebar ,geht die Implementierung folgenderma�en vor:
... 


Aufgabe A.4

> type RabattierterPreis = EUR

> guenstigste_Lieferanten_im_Lieferfenster ::
>   Suchanfrage -> Lieferfenster -> Stueckzahl 
>   -> [(Lieferantenname,RabattierterPreis)]

> guenstigste_Lieferanten_im_Lieferfenster = error "noch nicht implementiert"


Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
... 
