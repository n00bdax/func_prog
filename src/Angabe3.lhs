> {-# OPTIONS_GHC -Wno-name-shadowing #-}
> {-# LANGUAGE InstanceSigs #-}
> module Angabe3 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollstaendigen Sie auch die vorgegebenen Kommentaranfaenge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
5. Hinweis: Kommentar- und Programmzeilen muessen durch mindestens eine Leerzeile getrennt sein!


Als erstes fuehren wir den Typalias Nat1 ein; seine Werte verwenden wir fuer
die Darstellung des Typs von Matrizen:

> type Nat1 = Int
> type Typ  = (Nat1,Nat1)

Matrizen modellieren wir als Listen von (Matrix-) Zeilen ueber entsprechenden
selbstdefierten Listentypen:

Zur Namenswahl: LE fuer `Letztes Element', E fuer `Element'

> data Zeile = LE Int                       
>              | E Int Zeile deriving Show

Zur Namenswahl: LZ fuer `Letzte Zeile, Z fuer `Zeile'

> data Matrix = LZ Zeile                       
>               | Z Zeile Matrix deriving Show  

Um mit Argumenten umzugehen, die keine Matrix darstellen oder im Typ nicht
zueinander passen, fuehren wir den Wert fehler als fehleranzeigenden Wert
ein (aufgrund unserer Festlegung von fehler bedeutet das, dass die Rueckgabe
dieses Werts gleichbedeutend mit dem Aufruf der Funktion error mit dem 
Argument "Argument(e) typfehlerhaft" ist und die Programmausfuehrung mit 
Ausgabe der Zeichenreihe "Argument(e) typfehlerhaft" endet).

> fehler = error "Argument(e) typfehlerhaft"

Abschliessend fuehren wir den algebraischen Datentyp Matrixtyp ein:

> data Matrixtyp = Matrix_vom_Typ Typ | KeineMatrix deriving (Eq,Show)


Aufgabe A.1

> matrixtyp :: Matrix -> Matrixtyp
> matrixtyp (LZ z) = Matrix_vom_Typ (1, getWidth z)
> matrixtyp (Z z zs) = checkHeight zs 2 (getWidth z)
>   where
>       checkWidth :: Zeile -> Int -> Bool
>       checkWidth _ 0 = False
>       checkWidth (LE _) 1 = True
>       checkWidth (E _ xs) n = checkWidth xs (n-1)
>       checkWidth _ _ = False

>       checkHeight :: Matrix -> Int -> Int-> Matrixtyp
>       checkHeight (LZ z) m n
>           | checkWidth z n = Matrix_vom_Typ(m,n)
>           | otherwise = KeineMatrix
>       checkHeight (Z z zs)m n
>           | checkWidth z n = checkHeight zs (m+1) n
>           | otherwise = KeineMatrix

> getWidth :: Zeile -> Int
> getWidth (E _ x) = 1 + getWidth x
> getWidth (LE _) = 1

> getHeight :: Matrix -> Int
> getHeight (LZ _) = 1
> getHeight (Z _ m) = 1 + getHeight m




Aufgabe A.2

> instance Eq Matrix where
>  (==) :: Matrix -> Matrix -> Bool
>  a==b
>   | matrixtyp a == KeineMatrix = fehler
>   | matrixtyp a /= matrixtyp b = fehler
>   | otherwise = compareMats a b

global helper function without checks

> compareMats :: Matrix -> Matrix -> Bool
> compareMats (LZ x)(LZ y)      = x==y
> compareMats (Z x xs)(Z y ys)   = x==y &&  compareMats xs ys
> compareMats _ _ = fehler

> instance Eq Zeile where
>  (==) :: Zeile -> Zeile -> Bool
>  a==b
>   | getWidth a /= getWidth b = fehler
>   | otherwise = compareRows a b

global helper function without checks

> compareRows :: Zeile -> Zeile -> Bool
> compareRows (LE x)(LE y)      = x==y
> compareRows (E x xs)(E y ys)   = x==y &&  compareRows xs ys
> compareRows _ _ = fehler



Knapp, aber gut nachvollziehbar geht die Instanzdeklaration fuer Eq folgendermassen vor:
...
 
Aufgabe A.3


global helper functions wihtout checks for
emelentwise calculations

> elemZ :: (Int->Int->Int) -> Zeile -> Zeile -> Zeile
> elemZ op (LE x)  (LE y)   = LE(op x y)
> elemZ op (E x xs)(E y ys) = E (op x y)(elemZ op xs ys)
> elemZ _ _ _ = fehler

> elemM :: (Zeile->Zeile->Zeile) -> Matrix -> Matrix -> Matrix
> elemM op (LZ x)  (LZ y)   = LZ(op x y)
> elemM op (Z x xs)(Z y ys) = Z (op x y)(elemM op xs ys)
> elemM _ _ _ = fehler



basic operations on matrices

> instance Num Zeile where

>  (+),(-) :: Zeile -> Zeile -> Zeile
>  a + b = elemZ (+) a b
>  a - b = elemZ (-) a b

>  abs :: Zeile -> Zeile
>  abs (LE x) = LE(abs x)
>  abs (E x xs) = E (abs x)(abs xs)

>  fromInteger :: Integer -> Zeile
>  fromInteger x = LE (fromInteger x)

>  (*) :: Zeile -> Zeile -> Zeile
>  m1 * m2 = error "(*) bleibt unimplementiert!"

>  negate :: Zeile -> Zeile
>  negate a = error "(*) bleibt unimplementiert!"

  negate (LE x) = LE(-x)
  negate (E x xs) = E (-x)(negate xs)

>  signum :: Zeile -> Zeile
>  signum m = error "signum bleibt unimplementiert!"

> instance Num Matrix where

>  (+),(-) :: Matrix -> Matrix -> Matrix
>  a+b
>   | matrixtyp a == KeineMatrix = fehler
>   | matrixtyp a /= matrixtyp b = fehler
>   | otherwise = elemM (+) a b

>  a-b
>   | matrixtyp a == KeineMatrix = fehler
>   | matrixtyp a /= matrixtyp b = fehler
>   | otherwise = elemM (-) a b

>  abs :: Matrix -> Matrix
>  abs a
>   | matrixtyp a == KeineMatrix = fehler
>   | otherwise = abs' a
>   where
>       abs' (LZ a)   = LZ(abs a)
>       abs' (Z a xs) = Z (abs a) (abs' xs)

>  fromInteger :: Integer -> Matrix
>  fromInteger z = LZ (LE (fromInteger z))


>  (*) :: Matrix -> Matrix -> Matrix
>  m1 * m2 = error "(*) bleibt unimplementiert!"

>  negate :: Matrix -> Matrix
>  negate a = error "(*) bleibt unimplementiert!"

  negate (LZ x) = LZ(negate x)
  negate (Z x xs) = Z (negate x)(negate xs)

>  signum :: Matrix -> Matrix
>  signum m = error "signum bleibt unimplementiert!"




mkz::Int->Zeile
mkz 1 = LE 1
mkz n = E n (mkz(n-1))

mkm::Int->Int->Matrix
mkm 1 n = LZ (mkz n)
mkm m n = Z (mkz n) (mkm (m-1) n)


