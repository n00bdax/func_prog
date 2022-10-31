{-# LANGUAGE InstanceSigs #-}
module Angabe2 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
-}

-- Fuer A.1 bis A.4

data Lotterielos = Treffer 
                   | Niete 
                   | Freilos deriving (Eq,Show)
 
data Liste = Schluss Lotterielos
             | Kopf Lotterielos Liste deriving (Eq,Show)

data Baum = Blatt Lotterielos
            | Gabel Baum Lotterielos Baum deriving (Eq,Show)

data Liste' = Schluss' Baum
              | Kopf' Baum Liste' deriving (Eq,Show)

data Baum' = Blatt' Liste
             | Gabel' Baum' Liste Baum' deriving (Eq,Show)

type Loswert    = Lotterielos
type Auswertung = Ordering

-- Aufgabe A.1
analysiere :: Liste -> Loswert -> Loswert -> Auswertung
analysiere = analyze
-- Aufgabe A.2
analysiere' :: Baum -> Loswert -> Loswert -> Auswertung
analysiere' = analyze
-- Aufgabe A.3
analysiere'' :: Liste' -> Loswert -> Loswert -> Auswertung
analysiere'' = analyze
-- Aufgabe A.4
analysiere''' :: Baum' -> Loswert -> Loswert -> Auswertung
analysiere''' = analyze

class DatStruct a where
  count :: a -> Lotterielos -> Int
  analyze :: a -> Loswert -> Loswert -> Auswertung
  analyze a first second
    | first == second = EQ
    | otherwise = compare (count a first)(count a second)

instance DatStruct Liste where
  count :: Liste -> Lotterielos -> Int
  count (Schluss x) y = fromEnum (x==y)
  count (Kopf x xs) y = count xs y + fromEnum (x==y)

instance DatStruct Liste' where
  count :: Liste' -> Lotterielos -> Int
  count (Schluss' x) y = count x y
  count (Kopf' x xs) y = count xs y + count x y
  
instance DatStruct Baum where
  count :: Baum -> Lotterielos -> Int
  count (Blatt x) y = fromEnum (x==y)
  count (Gabel a x b) y = count a y +  fromEnum (x==y) + count b y

instance DatStruct Baum' where
  count :: Baum' -> Lotterielos -> Int
  count (Blatt' x) y = count x y
  count (Gabel' a x b) y = count a y + count x y + count b y
  

{-
analysiere* vergleicht Werte
count zaehlt Werte ab

-}

--debug variables
{-
l1::Liste
l1= Kopf Freilos(Kopf Treffer (Kopf Treffer (Kopf Niete (Schluss Niete))))
-- 2 Treffer, 1 Freilos, 2 Niete

l2::Liste
l2= Kopf Freilos(Kopf Treffer (Kopf Treffer (Kopf Niete (Kopf Niete(Kopf Treffer (Kopf Treffer (Kopf Niete (Schluss Niete))))))))   
-- 4 Treffer, 1 Freilos, 4 Niete

l3 :: Liste
l3  = Schluss Freilos
-- 1 Niete

b1::Baum
b1= Gabel (Blatt Treffer) Niete (Gabel (Blatt Treffer) Treffer (Blatt Freilos))
-- 3 Treffer, 1 Freilos, 1 Niete

b2::Baum
b2= Gabel (Blatt Niete) Niete (Gabel (Blatt Niete) Treffer (Gabel (Blatt Freilos) Freilos (Blatt Freilos)))
-- 1 Treffer, 3 Freilos, 3 Niete

b3 ::Baum
b3= Blatt Niete
--1 Niete

l1'::Liste'
l1' = Kopf' b1(Schluss' b1)
-- 6 Treffer, 2 Freilos, 2 Niete

l2'::Liste'
l2' = Kopf' b1(Kopf' b3 (Schluss' b3))
-- 3 Treffer, 1 Freilos, 3 Niete

b1'::Baum'
b1' = Blatt' l1
-- 2 Treffer, 1 Freilos, 2 Niete

b2'::Baum'
b2' = Gabel' (Blatt' l1) l2 (Gabel' (Blatt' l2) l2 (Blatt' l1))
-- 14 Treffer, 1 Freilos, 14 Niete
-}

