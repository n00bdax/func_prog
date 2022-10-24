module TestSuite1 where

import Angabe1 hiding (main, spec)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

import Data.List
import Data.Ord


--masses :: Gewichtsverzeichnis
--masses = [('m',1),('a',2),('s',3),('s',4),('e',5),('s',6)]
--
--masseses :: Gewichtsverzeichnis
--masseses = [('m',1),('a',2),('s',3),('s',4),('e',5),('s',6),('m',1),('a',2),('s',3),('s',4),('e',5),('s',6)]
--
--pl::Gewichtsverzeichnis
--pl = [('a',2),('b',1),('c',3),('d',2),('e',3)]
--
--mss::Gewichtsverzeichnis
--mss=[('a',0),('l',1)]
--
--mnn::Gewichtsverzeichnis
--mnn= [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7),('h',8),('i',9),('j',1)]
--
--ad14 :: Gewichtsverzeichnis
--ad14 = [(a,b)|b<-[1..4],a<-['a'..'d']]
--
--s1 :: Zeichenreihe
--s1 = "The quick brown fox jumps over the lazy dog"
--
--s2 :: Zeichenreihe
--s2 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." :: Zeichenreihe
--
--main :: IO ()
--main = defaultMainWithIngredients [consoleTestReporter] spec



spec :: TestTree
spec =
  testGroup
    "Angabe1"
    [ haeufigkeitTests,
      gewichtTests,
      korrigiereTests,
      korrigiereTests
    ]

haeufigkeitTests :: TestTree
haeufigkeitTests =
  testGroup
    "haeufigkeit Tests"
    [ testCase "haeufigkeit Test a" $
        haeufigkeit "Paparazzipizzeria" @?= [('P',1),('a',4),('p',2),('r',2),('z',4),('i',3),('e',1)],
      testCase "haeufigkeit Test b" $
        haeufigkeit "testing the test-system" @?= [('t',6),('e',4),('s',4),('i',1),('n',1),('g',1),(' ',2),('h',1),('-',1),('y',1),('m',1)],
      testCase "haeufigkeit Test c" $
        haeufigkeit "The quick brown fox jumps over the lazy dog" @?= [('T',1),('h',2),('e',3),(' ',8),('q',1),('u',2),('i',1),('c',1),('k',1),('b',1),('r',2),('o',4),('w',1),('n',1),('f',1),('x',1),('j',1),('m',1),('p',1),('s',1),('v',1),('t',1),('l',1),('a',1),('z',1),('y',1),('d',1),('g',1)],
      testCase "haeufigkeit Test d" $
        haeufigkeit "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
 @?= [('L',1),('o',29),('r',22),('e',37),('m',17),(' ',68),('i',42),('p',11),('s',18),('u',28),('d',18),('l',21),('t',32),('a',29),(',',4),('c',16),('n',24),('g',3),('b',3),('q',5),('.',4),('U',1),('v',3),('x',3),('D',1),('h',1),('f',3),('E',1)]
    ]

gewichtTests :: TestTree
gewichtTests =
  testGroup
    "Fibonacci Tests"
    [ testCase "gewicht Test a" $
        gewicht "anything" [('m',1),('a',2),('s',3),('s',4),('e',5),('s',6)] @?= -1,
      testCase "gewicht Test b" $
        gewicht " " [('x',3),('x',4)] + gewicht " " [] @?= -1,
      testCase "gewicht Test c" $
        gewicht "Paparazzipizzeria" [('a',0),('l',1)] @?= 0,
      testCase "gewicht Test d" $
        gewicht "Paparazzipizzeria" [('a',1),('l',0)] @?= 4
    ]

korrigiereTests :: TestTree
korrigiereTests =
  testGroup
    "korrigiere Tests"
    [ testCase "korrigiere Test a" $
        korrigiere [('a',1),('a',1),('a',1)] @?= [('a',1)],
      testCase "korrigiere Test b" $
        korrigiere [(a,b)|b<-[1..4],a<-['a'..'d']] @?= [('a',1),('b',1),('c',1),('d',1)],
      testCase "korrigiere Test c" $
        korrigiere [('m',1),('a',2),('s',3),('s',4),('e',5),('s',6)] @?= [('m',1),('a',2),('s',3),('e',5)],
      testCase "korrigiere Test d" $
        korrigiere' [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7),('h',8),('i',9),('j',1)] @?= [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7),('h',8),('i',9),('j',1)]
    ]

korrigiere'Tests :: TestTree
korrigiere'Tests =
  testGroup
    "korrigiere' Tests"
    [ testCase "korrigiere' Test a" $
        korrigiere' [('a',1),('a',1),('a',1)] @?= [('a',3)],
      testCase "korrigiere' Test b" $
        korrigiere' [(a,b)|b<-[1..4],a<-['a'..'d']] @?= [('a',10),('b',10),('c',10),('d',10)],
      testCase "korrigiere' Test c" $
        korrigiere' [('m',1),('a',2),('s',3),('s',4),('e',5),('s',6)] @?= [('m',1),('a',2),('s',13),('e',5)],
      testCase "korrigiere' Test d" $
        korrigiere' [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7),('h',8),('i',9),('j',1)] @?= [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7),('h',8),('i',9),('j',1)]
    ]
