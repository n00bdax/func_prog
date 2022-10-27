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
>   | matrixtyp b == KeineMatrix = fehler
>   | otherwise = cMat a b
>   where
>   cMat :: Matrix -> Matrix -> Bool
>   cMat (LZ x)(LZ y)      = cRow x y
>   cMat (Z x xs)(Z y ys)   = cRow x y && cMat xs ys
>   cMat _ _ = fehler

>   cRow :: Zeile -> Zeile -> Bool
>   cRow (LE x)(LE y)       = x == y
>   cRow (E x xs)(E y ys)   = x == y && cRow xs ys
>   cRow _ _ = fehler

handling faulty matrix structure mid operation is more
robust when handling infinitely recursing matrices
 - still breaks if the first row of m1 is infinite

>
>  (/=) :: Matrix -> Matrix -> Bool
>  m1 /= m2 = not (m1==m2)


Knapp, aber gut nachvollziehbar geht die Instanzdeklaration fuer Eq folgendermassen vor:
...
 
Aufgabe A.3


basic operations on lines and matrices

> instance Num Zeile where
>  (+) :: Zeile -> Zeile -> Zeile
>  (LE x) + (LE y)      = LE (x + y)
>  (E x xs) + (E y ys)  = E (x + y)(xs+ys)
>  (LE _) + (E _ _) = fehler
>  (E _ _) + (LE _) = fehler

>  (-) :: Zeile -> Zeile -> Zeile
>  (LE x) - (LE y)      = LE (x - y)
>  (E x xs) - (E y ys)  = E (x - y)(xs-ys)
>  (LE _) - (E _ _) = fehler
>  (E _ _) - (LE _) = fehler

>  abs :: Zeile -> Zeile
>  abs (LE x) = LE(abs x)
>  abs (E x xs) = E (abs x) (abs xs)

>  fromInteger :: Integer -> Zeile
>  fromInteger x = LE (fromIntegral x)

>  (*) :: Zeile -> Zeile -> Zeile
>  m1 * m2 = error "(*) bleibt unimplementiert!"

>  negate :: Zeile -> Zeile
>  negate m = error "negate bleibt unimplementiert!"

>  signum :: Zeile -> Zeile
>  signum m = error "signum bleibt unimplementiert!"

> instance Num Matrix where

>  (+) :: Matrix -> Matrix -> Matrix
>  a+b
>   | matrixtyp a == KeineMatrix = fehler
>   | otherwise = plus a b
>   where 
>   plus (LZ x)(LZ y)    = LZ (x + y)
>   plus (Z x xs)(Z y ys)= Z (x + y)(plus xs ys)
>   plus (LZ _)  (Z _ _) = fehler
>   plus (Z _ _)  (LZ _) = fehler

>  (-) :: Matrix -> Matrix -> Matrix
>  a-b
>   | matrixtyp a == KeineMatrix = fehler
>   | otherwise = minus a b
>   where
>   minus (LZ x)(LZ y)     = LZ (x - y)
>   minus (Z x xs)(Z y ys) = Z (x - y)(minus xs ys)
>   minus (LZ _)(Z _ _) = fehler
>   minus (Z _ _)(LZ _) = fehler

>  abs :: Matrix -> Matrix
>  abs a
>   | matrixtyp a == KeineMatrix = fehler
>   | otherwise = abs' a
>   where
>       abs' (LZ a)   = LZ(abs a)
>       abs' (Z a xs) = Z (abs a) (abs xs)



a

>  fromInteger :: Integer -> Matrix
>  fromInteger z = LZ (LE (fromIntegral z))


>  (*) :: Matrix -> Matrix -> Matrix
>  m1 * m2 = error "(*) bleibt unimplementiert!"

>  negate :: Matrix -> Matrix
>  negate m = error "negate bleibt unimplementiert!"

>  signum :: Matrix -> Matrix
>  signum m = error "signum bleibt unimplementiert!"







