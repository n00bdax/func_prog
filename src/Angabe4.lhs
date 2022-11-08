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

> data Waschmaschinentyp   = WM_Typ1 | WM_Typ2 | WM_Typ3 | WM_Typ4 | WM_Typ5
>   deriving (Eq, Bounded,Enum,Show)
> data Waeschetrocknertyp  = WT_Typ1 | WT_Typ2 | WT_Typ3 | WT_Typ4
>   deriving (Eq, Bounded,Enum,Show)
> data Waescheschleudertyp = WS_Typ1 | WS_Typ2 | WS_Typ3
>   deriving (Eq, Bounded,Enum,Show)

> data Typ =   WM Waschmaschinentyp
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

> type Lieferanten = Lieferantenname -> Sortiment

> type Suchanfrage = Typ

----------------------------------------------------------------------------------
                                    utilities
----------------------------------------------------------------------------------

> lfrntn :: Lieferantenliste
> lfrntn = [minBound..maxBound]

getter functions for Datensatz

> getPrice :: Datensatz -> Nat1
> getPrice (DS x _ _ _) = x
> getPrice _ = 0

> getPriceRed :: (Fractional a) => Datensatz -> a
> getPriceRed (DS x _ _ DreiProzent)  = fromIntegral x * 0.97
> getPriceRed (DS x _ _ FuenfProzent) = fromIntegral x * 0.95
> getPriceRed (DS x _ _ ZehnProzent)  = fromIntegral x * 0.9
> getPriceRed (DS x _ _ _)  = fromIntegral x
> getPriceRed _ = 0

> getInStock :: Datensatz -> Nat0
> getInStock (DS _ x _ _) = x
> getInStock _ = 0

> isInStock :: Datensatz -> Bool
> isInStock (DS _ x _ _) = x>0
> isInStock _ = False

> getInStockBy :: Datensatz -> (Lieferfenster -> Nat0)
> getInStockBy (DS _ _ x _) = x
> getInStockBy _ = const 0

> getSkonto :: Datensatz -> Skonto
> getSkonto (DS _ _ _ x)  =x
> getSkonto _ = Kein_Skonto


> toData :: Lieferanten -> Lieferantenname -> Typ -> Datensatz
> toData a n t = extract (a n) t
>   where
>   extract (WMS {wm=f}) (WM t) = f t
>   extract (WTS {wt=f}) (WT t) = f t
>   extract (WSS {ws=f}) (WS t) = f t
>   extract _ _ = DS 0 0 (const 0) Kein_Skonto

> drop2 :: [(a, b)] -> [a]
> drop2 ((x,_):xs) = x:drop2 xs
> drop2 _ = []

> drop3 :: [(a, b, c)] -> [(a, b)]
> drop3 ((x,y,_):xs) = (x,y):drop3 xs
> drop3 _ = []

> trim2Min :: [(a, EUR)] -> [(a, EUR)]
> trim2Min x
>   | isNothing(minV x) = []
>   | otherwise = let val = fromJust (minV x) in filter (\y -> snd y == val) x

> minV :: [(a, EUR)] -> Maybe EUR
> minV [] = Nothing

 minV ((_,EUR 0):xs) = minV xs

> minV [(_,x)] = Just x
> minV ((_,x):xs)
>   | isNothing(minV xs) = Just x 
>   | otherwise = min (Just x) (minV xs)

> e2 :: (a,b,c) -> b
> e2    (_,x,_)  = x
> e3 :: (a,b,c) -> c
> e3    (_,_,x)  = x

----------------------------------------------------------------------------------
                                end utilities
----------------------------------------------------------------------------------


Aufgabe A.1


> sofort_erhaeltlich_bei :: Suchanfrage -> Lieferanten  -> Lieferantenliste
> sofort_erhaeltlich_bei typ a = checkFor typ a lfrntn
>   where
>   checkFor :: Typ -> Lieferanten -> Lieferantenliste -> Lieferantenliste
>   checkFor typ a (x:xs)
>     | isInStock $ toData a x typ= x : checkFor typ a xs
>     | otherwise = checkFor typ a xs
>   checkFor _ _ _ = []


Knapp, aber gut nachvollziebar, geht die Implementierung folgendermaßen vor:
...


Aufgabe A.2

> type Stueckzahl  = Nat0
> type Gesamtpreis = Nat0

> sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Lieferanten -> (Stueckzahl,Gesamtpreis)
> sofort_erhaeltliche_Stueckzahl typ a = checkFor typ a lfrntn
>   where

>   checkFor :: Typ -> Lieferanten -> Lieferantenliste -> (Stueckzahl,Gesamtpreis)
>   checkFor typ a (x:xs) = let s = getInStock(toData a x typ)
>                               p = getPrice(toData a x typ)
>                           in add (s, s*p) $ checkFor typ a xs
>   checkFor _ _ _ = (0,0)

>   add :: (Stueckzahl,Gesamtpreis) -> (Stueckzahl,Gesamtpreis) -> (Stueckzahl,Gesamtpreis)
>   add (a,c) (b,d) = (a+b,c+d)
        
Knapp, aber gut nachvollziebar, geht die Implementierung folgendermaßen vor:
...

Aufgabe A.3

> type Preis = EUR
> guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Lieferanten -> Maybe Lieferantenliste
> guenstigste_Lieferanten typ lff a = maybefy $ map fst $ trim2Min $ getPriceList lfrntn typ lff a
>   where

>   maybefy :: [a] -> Maybe [a]
>   maybefy [] = Nothing
>   maybefy a  = Just a

>   getPriceList :: Lieferantenliste -> Typ -> Lieferfenster -> Lieferanten -> [(Lieferantenname, Preis)]
>   getPriceList (x:xs) typ lff a = let d = toData a x typ in
>       if getInStockBy d lff >0
>       then (x, EUR $ getPrice d ) : getPriceList (x:xs) typ lff a
>       else getPriceList (x:xs) typ lff a
>   getPriceList _ _ _ _ = []
>       


Knapp, aber gut nachvollziebar ,geht die Implementierung folgendermaßen vor:
... 


Aufgabe A.4

> type RabattierterPreis = EUR

> guenstigste_Lieferanten_im_Lieferfenster ::  Suchanfrage -> Lieferfenster -> Stueckzahl -> Lieferanten -> [(Lieferantenname,RabattierterPreis)]
> guenstigste_Lieferanten_im_Lieferfenster typ lff n a =  trim2Min $ drop3 $ filter (\x -> e3 x >= n) $ stockListRed lfrntn typ lff a n
>   where


>   stockListRed :: [Lieferantenname] -> Typ -> Lieferfenster -> Lieferanten -> Stueckzahl ->[(Lieferantenname, RabattierterPreis, Stueckzahl)]
>   stockListRed (x:xs) typ lff a n
>     = let d = toData a x typ in (x, EUR (ceiling  $ getPriceRed d * fromIntegral n), getInStockBy d lff) : stockListRed xs typ lff a n
>   stockListRed _ _ _ _ _ = []
> 


Knapp, aber gut nachvollziebar, geht die Implementierung folgendermaßen vor:










avilability patterns for testing 

        stocked:  restocks:
availA  10        never
availB  3         Q1
availC  4         Q3
availD  5         Q1,Q2,Q3,Q4

  > avail0, availA, availB, availC, availD :: Lieferfenster -> Nat0
  > avail0 _ = 0
  > availA _ = 10
  > availB (LF _  y) = 3 + y - 2023
  > availC (LF Q3 y) = 4 + y - 2023 +1
  > availC (LF Q4 y) = 4 + y - 2023 +1
  > availC (LF _  y) = 4 + y - 2023
  > availD (LF Q1 y) = 5 + (y - 2023)*4
  > availD (LF Q2 y) = 5 + (y - 2023)*4 +1
  > availD (LF Q3 y) = 5 + (y - 2023)*4 +2
  > availD (LF Q4 y) = 5 + (y - 2023)*4 +3
  
  > ds1,ds2,ds3,ds4,ds0 :: Datensatz
  > ds0 = DS   0  0 avail0 Kein_Skonto
  > ds1 = DS 149 10 availA DreiProzent
  > ds2 = DS 199  3 availB FuenfProzent
  > ds3 = DS 123  4 availC ZehnProzent
  > ds4 = DS   4  5 availD Kein_Skonto
  
  > wm1, wm2 :: Waschmaschinentyp -> Datensatz
  > wm1 WM_Typ1 = ds1
  > wm1 WM_Typ2 = ds2
  > wm1 WM_Typ3 = ds3
  > wm1 WM_Typ4 = ds4
  > wm1 WM_Typ5 = ds4
  > wm2 WM_Typ1 = ds3
  > wm2 _ = ds0
  
  
  > wt1, wt2 :: Waeschetrocknertyp -> Datensatz
  > wt1 WT_Typ1 = ds1
  > wt1 WT_Typ2 = ds2
  > wt1 WT_Typ3 = ds3
  > wt1 WT_Typ4 = ds4
  > wt2 WT_Typ1 = ds3
  > wt2 _ = ds0
  
  
  > ws1, ws2 :: Waescheschleudertyp -> Datensatz
  > ws1 WS_Typ1 = ds1
  > ws1 WS_Typ2 = ds2
  > ws1 WS_Typ3 = ds3
  > ws2 WS_Typ1 = ds3
  > ws2 _ = ds0
  
  > sWM1,sWM2,sWT1,sWT2,sWS1,sWS2 :: Sortiment
  > sWM1= WMS wm1
  > sWM2= WMS wm2
  > sWT1= WTS wt1
  > sWT2= WTS wt2
  > sWS1= WSS ws1
  > sWS2= WSS ws2
  
  > emptySort :: Sortiment
  > emptySort = WMS wm0
  
  > wm0 :: Waschmaschinentyp -> Datensatz
  > wm0 _ = ds0
  
  > l1 :: Lieferanten
  > l1 L1 = sWM1
  > l1 L2 = sWM2
  > l1 L3 = sWT1
  > l1 L4 = emptySort
  > l1 L5 = sWS1
  > l1 L6 = sWS2
  > l1 L7 = sWM1
  > l1 _ = sWM2
  
  > lff1, lff2, lff3, lff4 :: Lieferfenster
  > lff1 = LF Q1 2023
  > lff2 = LF Q2 2024
  > lff3 = LF Q3 2042
  > lff4 = LF Q4 3000



... 