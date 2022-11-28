{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}
{-# HLINT ignore "Use infix" #-}
module Angabe7 where
import           Data.Bifunctor
import           Data.Maybe


{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
   6. Kopieren Sie Ihre Implementierungen von Angabe 3 bzw. 6 an den
      entsprechenden Stellen ein. Beachten Sie, dass dafür drei Umbennennungen
      erforderlich sind, um Namenskonflikte zwischen Bezeichnungen von
      Angabe 3 und 6 zu vermeiden.
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

newtype EUR  = EUR { euro :: Nat1 }
  deriving (Eq, Ord, Show)

data Skonto  = Kein_Skonto
               | DreiProzent
               | FuenfProzent
               | ZehnProzent
  deriving (Eq, Ord, Show, Enum, Bounded)

data Waschmaschine    = M1 | M2 | M3 | M4 | M5
  deriving (Eq, Ord, Show, Enum, Bounded)
data Waeschetrockner  = T1 | T2 | T3 | T4
  deriving (Eq, Ord, Show, Enum, Bounded)
data Waescheschleuder = S1 | S2 | S3
  deriving (Eq, Ord, Show, Enum, Bounded)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder
  deriving (Eq, Ord, Show)

instance Enum Typ where
  toEnum :: Int -> Typ
  toEnum x
    | x < limit1 = M (toEnum x :: Waschmaschine)
    | x < limit2 = T ((toEnum $ x - limit1) :: Waeschetrockner)
    | otherwise = S ((toEnum $ x - limit2) :: Waescheschleuder)
    where
      limit1 = 1 + fromEnum (maxBound :: Waschmaschine)
      limit2 = 1 + fromEnum (maxBound :: Waeschetrockner) + limit1
  fromEnum :: Typ -> Int
  fromEnum (M x) = fromEnum x
  fromEnum (T x) = fromEnum x + 1 + fromEnum (M (maxBound :: Waschmaschine))
  fromEnum (S x) = fromEnum x + 1 + fromEnum (T (maxBound :: Waeschetrockner))

instance Bounded Typ where
  minBound :: Typ
  minBound = M minBound
  maxBound :: Typ
  maxBound = S maxBound

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show,Enum, Bounded)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr
                        }
  deriving (Eq, Show)

instance Ord Lieferfenster where
  (<=) :: Lieferfenster -> Lieferfenster -> Bool
  (<=) a b = fromEnum a <= fromEnum b

instance Enum Lieferfenster where
  toEnum :: Int -> Lieferfenster
  toEnum n = LF (toEnum (mod n 4)) (2023 + div n 4)

  fromEnum :: Lieferfenster -> Int
  fromEnum (LF q y) = (y-2023) * 4 + fromEnum q

data Datensatz
   = DS { preis_in_euro                        :: Nat1,
          sofort_lieferbare_stueckzahl         :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto                               :: Skonto
        }
     | Nicht_im_Sortiment

data Datensatz'
   = DS' { preis_in_euro'                        :: Nat1,
           sofort_lieferbare_stueckzahl'         :: Nat0,
           lieferbare_stueckzahl_im_Zeitfenster' :: Lieferausblick',
           skonto'                               :: Skonto
        }
     | Nicht_im_Sortiment'
  deriving (Eq, Ord, Show)


data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10
  deriving (Eq, Ord, Show, Enum, Bounded)


data Betroffen = Betroffen | NichtBetroffen
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Betroffene_Haendler = BH (Haendler -> Betroffen)

type AbLieferfenster = Lieferfenster

newtype Lieferausblick  = LA (Lieferfenster -> Nat0)
newtype Lieferausblick' = LA' [(Lieferfenster,Nat0)]
  deriving (Eq, Ord, Show)

newtype Sortiment  = Sort (Typ -> Datensatz)
newtype Sortiment' = Sort' [(Typ,Datensatz')]
  deriving (Eq, Ord, Show)

newtype Markt  = Mt (Haendler -> Sortiment)
newtype Markt' = Mt' [(Haendler,Sortiment')]
  deriving (Eq, Ord, Show)

-- Aufgabe A.1

lst2fkt_la :: [(Lieferfenster,Nat0)] -> (Lieferfenster -> Nat0)
lst2fkt_la = lst2fkt id

lst2fkt_so :: [(Typ,Datensatz')] -> (Typ -> Datensatz)
lst2fkt_so = lst2fkt (\case (DS' a b c d) -> DS a b (lst2fkt_la' c) d
                            _             -> error "unvollständiger Datensatz")

lst2fkt_ab :: [(Haendler,Sortiment')] -> (Haendler -> Sortiment)
lst2fkt_ab = lst2fkt lst2fkt_so'

-- Aufgabe A.2

lst2fkt_la' :: Lieferausblick' -> Lieferausblick
lst2fkt_la' (LA' x) = LA $ lst2fkt_la x

lst2fkt_so' :: Sortiment' -> Sortiment
lst2fkt_so' (Sort' x) = Sort $ lst2fkt_so x

lst2fkt_ab' :: Markt' -> Markt
lst2fkt_ab' (Mt' x) = Mt $ lst2fkt_ab x

-- Aufgabe A.4

preisanpassung :: Markt -> Markt
preisanpassung markt =
    reconM
  . map (second
  $ map (\(x,y)->
    let v = lookup x minPriceList
    in case v of
      Nothing -> (x,y)
      Just z  -> (x, sPrice y z)))
  . deconM
  $ markt

  where
    minPriceList :: [(Typ,Nat1)]
    minPriceList = mapMaybe (getMins
                  [(n1, gPrice n2) | (_,m2) <- deconM markt
                                   , (n1,n2) <- m2
                                  --  , 0 < gStock n2
                  ]) tList

    getMins :: [(Typ,Nat1)] -> Typ -> Maybe (Typ,Nat1)
    getMins l t = let specific = map snd . filter ((==t) . fst) $ l
                   in case specific of
                      x:xs -> Just (t,foldl min x xs)
                      _    -> Nothing

-- Aufgabe A.5

berichtige :: Markt -> Betroffene_Haendler -> AbLieferfenster -> Markt
berichtige markt (BH bh) al =
  reconM
  . map (\(a,b)->
      if bh a == Betroffen
      then (a,map (\(c,d) -> (c,noStockFrom d al)) b)
      else (a,b))
  . deconM
  $ markt

-- new structure because the default sucks
lst2fkt :: Eq a => (t -> b) -> [(a, t)] -> a -> b
lst2fkt f a b = (\case Just x -> f x
                       _      -> error "undefiniert") $ lookup b a

deconM :: Markt -> [(Haendler, [(Typ, Datensatz)])]
deconM (Mt x) = map (\a->(a,deconS$ x a)) hList

reconM :: [(Haendler, [(Typ, Datensatz)])] -> Markt
reconM = Mt . lst2fkt id . map (second reconS)

deconS :: Sortiment -> [(Typ, Datensatz)]
deconS (Sort x) = map (\a->(a,x a)) tList

reconS :: [(Typ, Datensatz)] -> Sortiment
reconS = Sort . lst2fkt id

-- lists
tList :: [Typ]
tList = [minBound .. maxBound]

hList :: [Haendler]
hList = [minBound .. maxBound]

lList :: [Lieferfenster]
lList = map toEnum [0..399]

-- helper functions

gPrice :: Datensatz -> Nat1
gPrice (DS x _ _ _) = x
gPrice _            = 0

sPrice :: Datensatz -> Nat1 -> Datensatz
sPrice (DS _ b c d) a = DS a b c d
sPrice _ _            = Nicht_im_Sortiment

gStock :: Datensatz -> Nat0
gStock (DS _ x _ _) = x
gStock _            = 0

noStockFrom :: Datensatz -> Lieferfenster -> Datensatz
noStockFrom (DS a b (LA c) d) t = DS a b (LA (\x-> if x<t then c x else 0)) d
noStockFrom _ _                 = Nicht_im_Sortiment









