> {-# HLINT ignore "Use camelCase" #-}
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
> matrixtyp (Z z m)
>   | not (equalWidths m (getWidth z)) = KeineMatrix
>   | otherwise = Matrix_vom_Typ (1 + getHeight m, getWidth z)
>   where
>       equalWidths :: Matrix -> Int -> Bool
>       equalWidths (LZ z) x = getWidth z == x
>       equalWidths (Z z m) x = getWidth z == x && equalWidths m x

> getWidth :: Zeile -> Int
> getWidth (E _ x) = 1 + getWidth x
> getWidth (LE _) = 1

> getHeight :: Matrix -> Int
> getHeight (LZ z) = 1
> getHeight (Z z m) = 1 + getHeight m

testvariablen
mMN ... matrix, M hoch, n breit, gueltig

m32:: Matrix
m32 = Z(E 1 (LE 1)) (Z (E 1 (LE 1)) (LZ (E 1 (LE 1))))
m32':: Matrix
m32' = Z(E 1 (LE 1)) (Z (E 1 (LE 1)) (LZ (E 1 (LE 1))))
m11:: Matrix
m11 = LZ (LE 1)
m11':: Matrix
m11' = LZ (LE 1)
m14:: Matrix
m14 = LZ (E 1(E 1(E 1 (LE 1))))
m14':: Matrix
m14' = LZ (E 1(E 1(E 1 (LE 1))))
m24:: Matrix
m24 = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 1(E 1(E 1 (LE 1)))))
m24':: Matrix
m24' = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 1(E 1(E 1 (LE 1)))))
n32:: Matrix
n32 = Z(E 2 (LE (-2))) (Z (E 1 (LE (-1))) (LZ (E (-10) (LE 1))))
n32':: Matrix
n32' = Z(E 1 (LE 1)) (Z (E 1 (LE 3)) (LZ (E 1 (LE 1))))
n11:: Matrix
n11 = LZ (LE 5)
n11':: Matrix
n11' = LZ (LE 10)
n14:: Matrix
n14 = LZ (E 1(E 1(E 1 (LE 21))))
n14':: Matrix
n14' = LZ (E 1(E 1(E 2 (LE 1))))
n24:: Matrix
n24 = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 3(E 1(E 1 (LE 1)))))
n24':: Matrix
n24' = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 1(E 1(E 3 (LE 1)))))
mk :: Matrix
mk = Z (E 1(E 1(LE 1))) (LZ (E 1(E 1(E 1 (LE 1)))))

Knapp, aber gut nachvollziebar geht matrixtyp folgendermassen vor: 
...

Aufgabe A.2

> instance Eq Matrix where
>  (==) :: Matrix -> Matrix -> Bool
>  m1 == m2 = cMat m1 m2
>   where
>       cMat :: Matrix -> Matrix -> Bool
>       cMat (LZ x)(LZ y)       = cRow x y
>       cMat (Z x xs)(Z y ys)   = cRow x y && cMat xs ys
>       cMat _ _ = fehler
>       cRow :: Zeile -> Zeile -> Bool
>       cRow (LE x)(LE y)       = x == y
>       cRow (E x xs)(E y ys)   = x == y && cRow xs ys
>       cRow _ _ = fehler
>
>  (/=) :: Matrix -> Matrix -> Bool
>  m1 /= m2 = not (m1==m2)

Knapp, aber gut nachvollziehbar geht die Instanzdeklaration fuer Eq folgendermassen vor:
...
 
Aufgabe A.3

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

> instance Num Matrix where

>  (+) :: Matrix -> Matrix -> Matrix
>  (LZ x)+(LZ y)        = LZ (x + y)
>  (Z x xs)+(Z y ys)    = Z (x + y)(xs+ys)
>  (LZ _) + (Z _ _) = fehler
>  (Z _ _) + (LZ _) = fehler

>  (-) :: Matrix -> Matrix -> Matrix
>  (LZ x)-(LZ y)        = LZ (x - y)
>  (Z x xs)-(Z y ys)    = Z (x - y)(xs-ys)
>  (LZ _) - (Z _ _) = fehler
>  (Z _ _) - (LZ _) = fehler

>  abs :: Matrix -> Matrix
>  abs x
>   | matrixtyp x == KeineMatrix = fehler
>   | otherwise = abs' x
>   where
>       abs' (LZ x)   = LZ(abs x)
>       abs' (Z x xs) = Z (abs x) (abs xs)

>  fromInteger :: Integer -> Matrix
>  fromInteger z = LZ (LE (fromIntegral z))

>  (*) :: Matrix -> Matrix -> Matrix
>  m1 * m2 = error "(*) bleibt unimplementiert!"

>  negate :: Matrix -> Matrix
>  negate m = error "negate bleibt unimplementiert!"

>  signum :: Matrix -> Matrix
>  signum m = error "signum bleibt unimplementiert!"


Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Num folgendermassen vor:
... 






