module Angabe1 where



{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
-}

-- Fuer A.1

type Nat0         = Int
type Zeichen      = Char
type Zeichenreihe = [Zeichen]
type Haeufigkeit  = Nat0
type Histogramm   = [(Zeichen,Haeufigkeit)]

-- Fuer A.2

type Gewicht        = Nat0
type Gewichtsverzeichnis = [(Zeichen,Gewicht)]
fehlerwert :: Gewicht -- why wasn't this type declared already?
fehlerwert = -1

-- Aufgabe A.1


histAdd :: Zeichen -> Histogramm -> Histogramm
histAdd c h
  | null h = [(c,1)]
  | c == fst (head h) = (fst (head h) ,snd (head h) +1):tail h
  | otherwise = head h : histAdd c (tail h)

extractChars :: Zeichenreihe -> Histogramm -> Histogramm
extractChars s h
  | null s = h
  | otherwise = extractChars (tail s) (histAdd (head s) h)

haeufigkeit :: Zeichenreihe -> Histogramm
haeufigkeit s = extractChars s []::Histogramm

{- Knapp, aber gut nachvollziehbar geht haufigkeit folgendermassen vor:
   
   String wird Element für Element betrachtet
   Histogramm wird auf der Suche nach einem entsprechenden key durchgegangen
   | gefunden = Eintragen durch Eintrag mit n+1 ersetzen
   | neuen Eintrag anlege 
   
-}



-- Aufgabe A.2

stringDuplicateCheck :: Zeichenreihe -> Bool
stringDuplicateCheck g
  | length g <= 1 = True
  | head g `elem` tail g = False
  | otherwise = stringDuplicateCheck (tail g)

registerDuplicateCheck :: Gewichtsverzeichnis -> Bool
registerDuplicateCheck g = stringDuplicateCheck[fst x | x <- g ]

gewicht:: Zeichenreihe -> Gewichtsverzeichnis -> Gewicht
gewicht z g
  | not (registerDuplicateCheck g) = fehlerwert
  | otherwise = sum[snd a | a<-g, b<-z , fst a == b]



{- Knapp, aber gut nachvollziehbar geht gewicht folgendermassen vor:
   ...
-}



-- Aufgabe A.3

fixRegister :: Gewichtsverzeichnis -> Gewichtsverzeichnis -> Gewichtsverzeichnis
fixRegister a b
  | null b = a
  | fst (head b) `elem` ([fst x | x<-a]) = fixRegister a (tail b)
  | otherwise = fixRegister (a++ [head b]) (tail b)

korrigiere :: Gewichtsverzeichnis -> Gewichtsverzeichnis
korrigiere g = fixRegister [head g] (tail g)

{- Knapp, aber gut nachvollziehbar geht gewicht folgendermassen vor:

if <betrachtetes Element> in <Anfang> then cut it out

-}



-- Aufgabe A.4

--korrigiere' :: Gewichtsverzeichnis -> Gewichtsverzeichnis
sumUp :: (Zeichen, Haeufigkeit) -> Gewichtsverzeichnis -> Gewichtsverzeichnis
sumUp a b = [(fst a, snd a + sum[snd x | x <- b , fst x == fst a])]

fixRegister'Deep:: Gewichtsverzeichnis -> Gewichtsverzeichnis -> Gewichtsverzeichnis
fixRegister'Deep a b
  | null b = a
  | otherwise = fixRegister' (init a ++ sumUp (last a) b) b

fixRegister' :: Gewichtsverzeichnis -> Gewichtsverzeichnis -> Gewichtsverzeichnis
fixRegister' a b
  | null b = a
  | fst (head b) `elem` ([fst x | x<-a]) = fixRegister' a (tail b)
  | otherwise = fixRegister'Deep (a ++ [head b]) (tail b)

korrigiere' :: Gewichtsverzeichnis -> Gewichtsverzeichnis
korrigiere' g = fixRegister'Deep [head g] (tail g)

{- Knapp, aber gut nachvollziehbar geht gewicht folgendermassen vor:

wie A.3, jedoch werden bei jedem erfolgreichen Schritt spätere, gleiche Elemente
auf das zuletzt betrachtete Element addiert

-}

-- Test Variables
--
--masses :: Gewichtsverzeichnis
--masses = [('m',1),('a',2),('s',3),('s',4),('e',5),('s',6)]
--
--masseses :: Gewichtsverzeichnis
--masseses = masses ++ masses
--
--pl::Gewichtsverzeichnis
--pl = [('a',2),('b',1),('c',3),('d',2),('e',3)]
--
--mss::Gewichtsverzeichnis
--mss=[('a',0),('l',1)]
--
--mnn::Gewichtsverzeichnis
--mnn= [('a',2),('b',1),('c',3),('d',2),('e',3),('a',2),('b',1),('c',3),('d',2),('e',3)]
--
--ad14 :: Gewichtsverzeichnis
--ad14 = [(a,b)|b<-[1..4],a<-['a'..'d']]
--
--s1 :: Zeichenreihe
--s1 = "The quick brown fox jumps over the lazy dog"::Zeichenreihe
--
--s2 :: Zeichenreihe
--s2 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." :: Zeichenreihe

