> {-# LANGUAGE LambdaCase #-}
> {-# OPTIONS_GHC -Wno-name-shadowing #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Use camelCase" #-}



> module Angabe4 where

> import Data.Maybe
  
1. Vervollständigen Sie gemaess Angabentext!
2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
3. Löschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisung!
4. Achten Sie darauf, dass `Gruppe' Leserechte für Ihre Abgabedatei hat!
5. Hinweis: Kommentar- und Programmzeilen müssen durch mindestens eine Leerzeile getrennt sein!


> type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
> type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
> type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

> newtype EUR  = EUR { euro :: Nat1 } deriving (Eq, Ord, Show)

> data Skonto  = Kein_Skonto 
>                | DreiProzent  
>                | FuenfProzent 
>                | ZehnProzent
>   deriving (Eq, Bounded, Enum, Show, Ord)

> data Waschmaschinentyp   = WM_Typ1 | WM_Typ2 | WM_Typ3 | WM_Typ4 | WM_Typ5
>   deriving (Eq, Bounded,Enum,Show)
> data Waeschetrocknertyp  = WT_Typ1 | WT_Typ2 | WT_Typ3 | WT_Typ4
>   deriving (Eq, Bounded,Enum,Show)
> data Waescheschleudertyp = WS_Typ1 | WS_Typ2 | WS_Typ3
>   deriving (Eq, Bounded,Enum,Show)

> data Typ =   WM Waschmaschinentyp
>            | WT Waeschetrocknertyp
>            | WS Waescheschleudertyp deriving (Eq, Show)

> data Quartal       = Q1 | Q2 | Q3 | Q4
>   deriving (Eq,Ord,Show,Enum, Bounded)
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

> type Lieferanten = Lieferantenname -> Sortiment

> type Suchanfrage = Typ

----------------------------------------------------------------------------------
                                    utilities
----------------------------------------------------------------------------------

iterable list of Lieferantenname

> list :: Lieferantenliste
> list = [minBound..maxBound]

getter functions for Datensatz

> gPrice :: Datensatz -> Nat1
> gPrice (DS x _ _ _) = x
> gPrice _ = 0

> gPriceRed :: Datensatz -> Double
> gPriceRed (DS x _ _ DreiProzent)    = fromIntegral x * 0.97
> gPriceRed (DS x _ _ FuenfProzent)   = fromIntegral x * 0.95
> gPriceRed (DS x _ _ ZehnProzent)    = fromIntegral x * 0.9
> gPriceRed (DS x _ _ _)  = fromIntegral x
> gPriceRed _ = 0

fromIntegral ist in Haskell notewndig um Int mit Double zu multiplizieren ...

> gStock :: Datensatz -> Nat0
> gStock (DS _ x _ _) = x
> gStock _ = 0

> gStockBy :: Datensatz -> (Lieferfenster -> Nat0)
> gStockBy (DS _ _ x _) = x
> gStockBy _ = const 0

> getSkonto :: Datensatz -> Skonto
> getSkonto (DS _ _ _ x)  =  x     
> getSkonto _ = Kein_Skonto

data extractor function

> toData :: Sortiment -> Typ -> Datensatz
> toData (WMS {wm=f}) (WM t) = f t
> toData (WTS {wt=f}) (WT t) = f t
> toData (WSS {ws=f}) (WS t) = f t
> toData _ _ = DS 0 0 (const 0) Kein_Skonto


miscellaneous helper functions

> trim2MinSnd :: [(a, EUR)] -> [(a, EUR)]
> trim2MinSnd x
>   | isNothing(minSnd x) = []
>   | otherwise = let minVal = fromJust (minSnd x)
>                 in filter (\y -> snd y == minVal) x
>   where
>   minSnd :: Ord b => [(a, b)] -> Maybe b
>   minSnd ((_,x):xs) = Just $ foldl min x (map snd xs) 
>   minSnd _ = Nothing

minSnd  
        returns the minimum value of the second elements for a list of tuples
        returns Nothing on empty lists

trim2MinSnd
        reduces list to elements matching the 
        global minimum on their second element



----------------------------------------------------------------------------------
                                end utilities
----------------------------------------------------------------------------------


Aufgabe A.1

> sofort_erhaeltlich_bei :: Suchanfrage -> Lieferanten  -> Lieferantenliste
> sofort_erhaeltlich_bei typ a = [x | x <- list, gStock(toData (a x) typ) > 0 ]


list comprehension starting with list of all Lieferantenname

gStock(toData (a x) typ) > 0
        only allows values having sofort_erhaeltliche_Stueckzahl > 0 








Aufgabe A.2

> type Stueckzahl  = Nat0
> type Gesamtpreis = Nat0

> sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Lieferanten -> (Stueckzahl,Gesamtpreis)
> sofort_erhaeltliche_Stueckzahl typ a =
>     foldl (\(a,b)(c,d)->(a+c,b+d)) (0,0) $
>     map (\q-> let d = toData (a q) typ in (gStock d, gStock d * gPrice d))
>     list


list    
        list of all lieferantennamen

map     
        calls a function on each element oif a list

        let d = toData (a q) typ in
                legt entsprechenden Datensatz zu dem Lieferantenname als d an

        (\q-> (gStock d, gStock d * gPrice d))
                Abbildung von Lieferantenname -> (Stueckzahl, Stueckzahl*Preis)

foldl   
        pushes a list into a calculation, from left to right
                foldl (+) 10 [1,4,3] z.B. results in  (((10 + 1) +4) +3) = 18

        (\ (a,b) (c,d) -> (a+c,b+d) )
                adds tuples








Aufgabe A.3

> type Preis = EUR
> guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Lieferanten -> Maybe Lieferantenliste
> guenstigste_Lieferanten typ lff a = (\x -> if null x then Nothing else Just x) $
>                                      map fst $
>                                      trim2MinSnd $
>                                      gStockListBy list typ a lff
>   where
>   gStockListBy :: Lieferantenliste -> Typ -> Lieferanten -> Lieferfenster ->  [(Lieferantenname, Preis)]
>   gStockListBy (x:xs) typ a lff =
>       let d = toData (a x) typ in 
>       if gStockBy d lff > 0
>       then (x, EUR $ gPrice d) : gStockListBy xs typ a lff
>       else gStockListBy xs typ a lff
>   gStockListBy _ _ _ _ = []


gStockListBy    
        builds [(Lieferantenname, Preis)] Lieferantenname,
        die eine Stueckzahl > 0 haben

trim2MinSnd
        reduce list to elements having a second
        element matching the minimum across the list

map fst
        reduce list of tuples to their first elements

(\x -> if null x then Nothing else Just x)
        replaces an empty List wit Nothing
        and turns everything else into a Just value








Aufgabe A.4

> type RabattierterPreis = EUR

> guenstigste_Lieferanten_im_Lieferfenster ::  Suchanfrage -> Lieferfenster -> Stueckzahl -> Lieferanten -> [(Lieferantenname,RabattierterPreis)]
> guenstigste_Lieferanten_im_Lieferfenster typ lff n a = 
>   trim2MinSnd $
>   map    (\(x,y,_) -> (x,y)) $
>   filter (\(_,_,x) -> x >= n) $ 
>   map    (\x -> let d = toData (a x) typ; p = gPriceRed d; s = gStockBy d lff
>                 in (x, EUR (ceiling $ p * fromIntegral n), s))
>   list


map
        calls a function on each element of a list

        let d = toData (a x) typ; p = gPriceRed d; s = gStockBy d lff in 
                sets d as Datensatz, p as RabattierterPreis, s as Stueckzahl
                for a given Lieferantenname and Typ
                

        (\x -> (x, EUR (ceiling ( p * fromIntegral n)), s))
                takes a Lieferantename and construct a thruple having
                (Lieferantennname, RabattierterPreis * desired Stueckzahl, available Stueckzahl)
                Gesaamtpreis ist aufgerundet
        
filter (\ (_,_,x) -> x >= n)
        reduces list of thruples to elements having a third element >= n

map (\ (x,y,_) -> (x,y))
        cuts thruple down to tuple

trim2MinSnd
        reduces list to elements matching the 
        global minimum on their second element