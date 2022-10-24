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

-- Aufgabe A.1
analysiere :: Liste -> Loswert -> Loswert -> Auswertung
analysiere l lw lw' = compare (cL l lw)(cL l lw')

cL :: Liste -> Lotterielos -> Int
cL (Schluss x) val = fromEnum (x==val)
cL (Kopf x xs) val = cL xs val + fromEnum (x==val)

-- Aufgabe A.2
analysiere' :: Baum -> Loswert -> Loswert -> Auswertung
analysiere' b lw lw' = compare (cT b lw)(cT b lw')

cL' :: Liste' -> Lotterielos -> Int
cL' (Schluss' x) val = cT x val
cL' (Kopf' x xs) val = cL' xs val + cT x val

-- Aufgabe A.3
analysiere'' :: Liste' -> Loswert -> Loswert -> Auswertung
analysiere'' l lw lw' = compare (cL' l lw)(cL' l lw')

cT :: Baum -> Lotterielos -> Int
cT (Blatt x) val = fromEnum (x==val)
cT (Gabel a x b) val = cT a val +  fromEnum (x==val) + cT b val

-- Aufgabe A.4
analysiere''' :: Baum' -> Loswert -> Loswert -> Auswertung
analysiere''' b lw lw' = compare (cT' b lw)(cT' b lw')

cT' :: Baum' -> Lotterielos -> Int
cT' (Blatt' x) val = cL x val
cT' (Gabel' a x b) val = cT' a val + cL x val + cT' b val

{-
analysiere* vergleicht Werte
cL berechnet Übereinstimmungen in einer linked list mit Variablen
cT berechnet Übereinstimmungen in einem tree mit Variablen
cL' summiert Übereinstimmungen in einer linked list aus trees (calls cT)
cT' summiert Übereinstimmungen in einem tree aus linked lists (calls cL)

-}
