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

{- Knapp, aber gut nachvollziehbar geht analysiere folgendermassen vor:
   ...
-}



-- Aufgabe A.2

analysiere' :: Baum -> Loswert -> Loswert -> Auswertung

{- Knapp, aber gut nachvollziehbar geht analysiere' folgendermassen vor:
   ...
-}



-- Aufgabe A.3

analysiere'' :: Liste' -> Loswert -> Loswert -> Auswertung

{- Knapp, aber gut nachvollziehbar geht analysiere'' folgendermassen vor:
   ...
-}



-- Aufgabe A.4

analysiere''' :: Baum' -> Loswert -> Loswert -> Auswertung

{- Knapp, aber gut nachvollziehbar geht analysiere''' folgendermassen vor:
   ...
-}
