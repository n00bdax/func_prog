{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Angabe5 where
import           Data.Maybe
import Data.Bifunctor

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

newtype EUR  = EUR { euro :: Nat1 }
   deriving (Eq,Ord,Show)

data Skonto  = Kein_Skonto
               | DreiProzent
               | FuenfProzent
               | ZehnProzent
               deriving (Eq, Ord, Show, Enum, Bounded)

data Waschmaschine    = M1 | M2 | M3 | M4 | M5
   deriving (Eq, Enum, Bounded, Show)
data Waeschetrockner  = T1 | T2 | T3 | T4
   deriving (Eq, Enum, Bounded, Show)
data Waescheschleuder = S1 | S2 | S3
   deriving (Eq, Enum, Bounded, Show)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder
   deriving (Eq, Show)


data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr
                        } deriving (Eq, Ord, Show)


data Datensatz
   = DS { preis_in_euro                        :: Nat1,
          sofort_lieferbare_stueckzahl         :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto                               :: Skonto }
     | Nicht_im_Sortiment deriving (Eq,Show)


newtype Lieferausblick = LA [(Lieferfenster,Nat0)] deriving (Eq,Show)
newtype Sortiment = Sort [(Typ,Datensatz)]         deriving (Eq,Show)
newtype Anbieter = A [(Haendler,Sortiment)]        deriving (Eq,Show)


data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10
   deriving (Eq, Enum, Bounded, Show)


type Suchanfrage = Typ

----------------- helper functions ----------------

toData :: Typ -> Anbieter -> [(Haendler, Datensatz)]
toData typ (A anbieter)= mapMaybe (\(x,Sort y) -> (\(v,w) -> if isNothing w then Nothing else Just (v,fromJust w))
                         ( x,(\v->  if null v then Nothing else Just (head v)) . map snd $ filter (\g -> fst g == typ) y )
                         ) anbieter


gPrice :: Datensatz -> Nat1
gPrice (DS x _ _ _) = x
gPrice _            = 0

gPriceRed :: Datensatz -> Double
gPriceRed (DS x _ _ DreiProzent)  = fromIntegral x * 0.97
gPriceRed (DS x _ _ FuenfProzent) = fromIntegral x * 0.95
gPriceRed (DS x _ _ ZehnProzent)  = fromIntegral x * 0.9
gPriceRed (DS x _ _ _)            = fromIntegral x
gPriceRed _                       = 0

gStock :: Datensatz -> Nat0
gStock (DS _ x _ _) = x
gStock _            = 0

gLA :: Datensatz -> Lieferausblick
gLA (DS _ _ x _) = x
gLA _            = LA []


gStockBy :: Datensatz -> Lieferfenster -> Stueckzahl
gStockBy (DS _ _ (LA x) _) lff = (\case []-> 0
                                        a  -> (snd $ head a)) $ filter (\a->fst a == lff) x
gStockBy _ _              = 0

getSkonto :: Datensatz -> Skonto
getSkonto (DS _ _ _ x) =  x
getSkonto _            = Kein_Skonto

trim2MinSnd :: Ord b => [(a, b)] -> [(a, b)]
trim2MinSnd ((x,y):xs) = filter (\a -> snd a == foldl min y (map snd xs)) ((x,y):xs)
trim2MinSnd _ = []


hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates (x:xs) = elem x xs || hasDuplicates xs
hasDuplicates _      = False

class Wgf a where                -- Wgf fuer `wohlgeformt'
 ist_wgf    :: a -> Bool         -- ist_wgf fuer `ist wohlgeformt'
 ist_nwgf   :: a -> Bool         -- ist_nwgf fuer `ist nicht wohlgeformt'
 wgf_fehler :: a -> b
 -- Protoimplementierungen
 ist_wgf x  = not (ist_nwgf x)
 ist_nwgf x = not (ist_wgf x)
 wgf_fehler = error "Argument fehlerhaft"


-- Aufgabe A.1

instance Wgf Lieferausblick where
   ist_wgf :: Lieferausblick -> Bool
   ist_wgf (LA (x:xs)) = subCheck x xs && ist_wgf (LA xs)
      where
         subCheck :: (Lieferfenster, Nat0) -> [(Lieferfenster, Nat0)] -> Bool
         subCheck (x1,x2) ((y1,y2):ys)
            | x1 == y1 = x2==y2 && subCheck (x1,x2) ys
            | otherwise = subCheck (x1,x2) ys
         subCheck _ _ = True
   ist_wgf _ = True

   ist_nwgf :: Lieferausblick -> Bool
   ist_nwgf = not.ist_wgf

   wgf_fehler :: Lieferausblick -> b
   wgf_fehler = error "Ausblickfehler"

instance Wgf Sortiment where
   ist_wgf :: Sortiment -> Bool
   ist_wgf = not.ist_nwgf

   ist_nwgf :: Sortiment -> Bool
   ist_nwgf (Sort x) = hasDuplicates (map fst x) ||  any (any(ist_nwgf. gLA)) x

   wgf_fehler :: Sortiment -> b
   wgf_fehler = error "Sortimentfehler"

instance Wgf Anbieter where
   ist_wgf :: Anbieter -> Bool
   ist_wgf  = not.ist_nwgf

   ist_nwgf :: Anbieter -> Bool
   ist_nwgf (A x) = hasDuplicates (map fst x) ||  any (ist_nwgf . snd) x

   wgf_fehler :: Anbieter -> b
   wgf_fehler = error "Anbieterfehler"




-- Aufgabe A.5

type Haendlerliste = [Haendler]

sofort_lieferfaehig :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig typ (A anbieter)
   | null anbieter = []
   | ist_nwgf (A anbieter) = wgf_fehler (A anbieter)
   | otherwise =  reverse . map fst . filter (\(_,x) -> gStock x > 0) $
                  toData typ (A anbieter)



-- Aufgabe A.6

type Stueckzahl  = Nat0
type Gesamtpreis = Nat0

sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
sofort_erhaeltliche_Stueckzahl typ (A anbieter)
   | null anbieter = (0,0)
   | ist_nwgf (A anbieter) = error "Anbieterargumentfehler"
   | otherwise =  foldl (\(a,b)(c,d)->(a+c,b+d))(0,0) .
                  map(\(_,x)-> (gStock x,gPrice x * gStock x)) $
                  toData typ (A anbieter)




-- Aufgabe A.7

type Preis = EUR
guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
guenstigste_Lieferanten typ lff (A anbieter)
   | null anbieter = Nothing
   | ist_nwgf (A anbieter) = error "Anbieterargumentfehler"
   | otherwise  = (\x -> if null x then Nothing else Just x) .
                  reverse . map fst . trim2MinSnd $
                  map (second gPrice) $ 
                  filter (\x -> gStockBy (snd x) lff> 0)$
                  toData typ (A anbieter)




-- Aufgabe A.8

type RabattierterPreis = EUR

guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler,RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster typ lff n (A anbieter)
   | null anbieter = []
   | ist_nwgf (A anbieter) = error "Anbieterargumentfehler"
   | otherwise  = reverse . trim2MinSnd .
                  map (\(x,y) -> (x,EUR $ (\a ->  10 * ceiling ( a/10)) (gPriceRed y * fromIntegral n))) .
                  filter (\(_,x) -> gStockBy x lff >= n) $
                  toData typ (A anbieter)



