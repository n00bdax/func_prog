{-# LANGUAGE LambdaCase #-}

import           Angabe4
import           Test.HUnit
main :: IO ()
main = runTestTTAndExit $ test tests

fenster1 :: Lieferfenster -> Nat0
fenster1 f = case f of
  (LF Q1 2023) -> 1
  (LF Q2 2023) -> 3
  (LF Q2 2025) -> 0
  (LF Q3 2025) -> 4
  (LF Q4 2025) -> 0
  (LF Q1 2026) -> 4
  (LF Q4 2026) -> 2
  _            -> 0

fenster2 :: Lieferfenster -> Nat0
fenster2 f = case f of
  (LF Q1 2023) -> 3
  (LF Q2 2023) -> 4
  (LF Q1 2025) -> 2
  _            -> 0

lieferanten :: Lieferanten
lieferanten name = case name of
  L1  -> WMS    (\case
       WM_Typ1 -> DS 69 0 fenster1 DreiProzent
       WM_Typ2 -> DS 75 2 fenster2 Kein_Skonto
       WM_Typ3 -> DS 101 2 fenster1 FuenfProzent
       WM_Typ4 -> DS 95 22 fenster2 Kein_Skonto
       WM_Typ5 -> DS 140 1 fenster1 DreiProzent)
  L2  -> WMS    (\case
       WM_Typ1 -> DS 65 1 fenster1 Kein_Skonto
       WM_Typ4 -> DS 99 2 fenster2 ZehnProzent
       WM_Typ5 -> DS 44 2 fenster2 DreiProzent
       _       -> Nicht_im_Sortiment)
  L3  -> WSS    (\case
       WS_Typ1 -> DS 117 2 fenster2 Kein_Skonto
       WS_Typ3 -> DS 103 0 fenster1 Kein_Skonto
       _       -> Nicht_im_Sortiment)
  L4  -> lieferanten L2
  L5  -> lieferanten L2
  L8  -> WSS    (\case
       WS_Typ1 -> DS 117 0 fenster2 DreiProzent
       _       -> Nicht_im_Sortiment)
  L9  -> WTS    (\case
       WT_Typ4 -> DS 250 0 fenster2 ZehnProzent
       _       -> Nicht_im_Sortiment)
  L10 -> lieferanten L1
  _   -> WMS (const Nicht_im_Sortiment)

lieferantenEmpty :: Lieferantenname -> Sortiment
lieferantenEmpty _ = WSS $ const Nicht_im_Sortiment

lieferantenEmptyFenster :: Lieferantenname -> Sortiment
lieferantenEmptyFenster _ = WTS (\t -> DS 130 0 (const 0) ZehnProzent)

tests =
  [ " A.1.1" ~: sofort_erhaeltlich_bei (WM WM_Typ5) lieferantenEmpty  ~?= []
  , " A.1.2" ~: sofort_erhaeltlich_bei (WM WM_Typ5) lieferanten       ~?= [L1, L2, L4, L5, L10]
  , " A.1.3" ~: sofort_erhaeltlich_bei (WS WS_Typ1) lieferanten       ~?= [L3]
  , " A.1.4" ~: sofort_erhaeltlich_bei (WS WS_Typ3) lieferanten       ~?= []
  , " A.1.5" ~: sofort_erhaeltlich_bei (WT WT_Typ4) lieferanten       ~?= []

  , " A.2.1" ~: sofort_erhaeltliche_Stueckzahl (WM WM_Typ2) lieferantenEmpty    ~?= (0, 0)
  , " A.2.2" ~: sofort_erhaeltliche_Stueckzahl (WM WM_Typ2) lieferanten         ~?= (4, 300)
  , " A.2.3" ~: sofort_erhaeltliche_Stueckzahl (WS WS_Typ1) lieferanten         ~?= (2, 117 * 2)
  , " A.2.4" ~: sofort_erhaeltliche_Stueckzahl (WS WS_Typ3) lieferanten         ~?= (0, 0)
  , " A.2.5" ~: sofort_erhaeltliche_Stueckzahl (WT WT_Typ3) lieferanten         ~?= (0, 0)

  , " A.3.1" ~: guenstigste_Lieferanten (WM WM_Typ1) (LF Q1 2023) lieferantenEmpty          ~?= Nothing
  , " A.3.2" ~: guenstigste_Lieferanten (WM WM_Typ1) (LF Q1 2023) lieferantenEmptyFenster   ~?= Nothing
  , " A.3.3" ~: guenstigste_Lieferanten (WM WM_Typ1) (LF Q1 2023) lieferanten               ~?= Just [L2, L4, L5]
  , " A.3.4" ~: guenstigste_Lieferanten (WM WM_Typ1) (LF Q2 2025) lieferanten               ~?= Nothing
  , " A.3.5" ~: guenstigste_Lieferanten (WM WM_Typ4) (LF Q1 2023) lieferanten               ~?= Just [L1, L10]
  , " A.3.6" ~: guenstigste_Lieferanten (WM WM_Typ1) (LF Q1 2060) lieferanten               ~?= Nothing
  , " A.3.7" ~: guenstigste_Lieferanten (WT WT_Typ4) (LF Q1 2025) lieferanten               ~?= Just [L9]
  , " A.3.8" ~: guenstigste_Lieferanten (WT WT_Typ4) (LF Q1 9999) lieferanten               ~?= Nothing

  , " A.4.1" ~: guenstigste_Lieferanten_im_Lieferfenster (WS WS_Typ1) (LF Q2 2023) 4   lieferantenEmpty        ~?= []
  , " A.4.2" ~: guenstigste_Lieferanten_im_Lieferfenster (WS WS_Typ1) (LF Q2 2023) 4   lieferantenEmptyFenster ~?= []
  , " A.4.3" ~: guenstigste_Lieferanten_im_Lieferfenster (WM WM_Typ1) (LF Q1 2023) 999 lieferanten ~?= []
  , " A.4.4" ~: guenstigste_Lieferanten_im_Lieferfenster (WM WM_Typ4) (LF Q1 2025) 3   lieferanten ~?= []
  , " A.4.5" ~: guenstigste_Lieferanten_im_Lieferfenster (WM WM_Typ4) (LF Q1 2025) 2   lieferanten ~?= [(L2, EUR 179), (L4, EUR 179), (L5, EUR 179)]
  , " A.4.6" ~: guenstigste_Lieferanten_im_Lieferfenster (WM WM_Typ4) (LF Q1 2025) 1   lieferanten ~?= [(L2, EUR 90), (L4, EUR 90), (L5, EUR 90)]
  , " A.4.7" ~: guenstigste_Lieferanten_im_Lieferfenster (WS WS_Typ1) (LF Q2 2023) 4   lieferanten ~?= [(L8, EUR 454)]]


