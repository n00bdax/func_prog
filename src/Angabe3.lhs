> module Angabe3 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
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
>              | E Int Zeile Liste deriving Show

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

> data Matrixtyp = Matrix_vom_Typ Typ 
                    | KeineMatrix deriving (Eq,Show)


Aufgabe A.1

 > matrixtyp :: Matrix -> Matrixtyp

Knapp, aber gut nachvollziebar geht matrixtyp folgendermassen vor: 
...


Aufgabe A.2

> instance Eq Matrix where
>  m1 == m2 = error "noch nicht implementiert!"
>  m1 /= m2 = error "noch nicht implementiert!"

Knapp, aber gut nachvollziehbar geht die Instanzdeklaration fuer Eq folgendermassen vor:
...
 

Aufgabe A.3

> instance Num Matrix where
>  m1 + m2 = error "noch nicht implementiert!"
>  m1 - m2 = error "noch nicht implementiert!"
>  abs m = error "noch nicht implementiert!"
>  fromInteger z = error "noch nicht implementiert!"
>  m1 * m2 = error "(*) bleibt unimplementiert!"
>  negate m = error "negate bleibt unimplementiert!"
>  signum m = error "signum bleibt unimplementiert!"

Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Num folgendermassen vor:
... 






