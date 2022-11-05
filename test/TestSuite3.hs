module TestSuite3 where

import           Angabe3
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main =
  defaultMainWithIngredients
    [consoleTestReporter]
    spec

spec :: TestTree
spec =
  testGroup
    "Angabe3"
    [ matrixtypTest,
      eqTest,
      plusTest,
      minusTest,
      absTest,
      mixedTest
      --    ,infiniteTest1 -- 5 rows, row 2 through 4 are infinite
      --    ,infiniteTest2 -- 5 rows, row 3 is infinite
      --    ,errorTest
      --    ,stressTest
    ]

properties :: TestTree
properties =
  testGroup
    "Properties"
    []

m31 :: Matrix
m22 :: Matrix
m32 :: Matrix
m32' :: Matrix
m11 :: Matrix
m11' :: Matrix
m14 :: Matrix
m14' :: Matrix
m24 :: Matrix
m24' :: Matrix
n32 :: Matrix
n32' :: Matrix
n11 :: Matrix
n11' :: Matrix
n14 :: Matrix
n14' :: Matrix
n24 :: Matrix
n24' :: Matrix
mk1 :: Matrix
mk2 :: Matrix
mk3 :: Matrix
infiniRow :: Zeile
infiniMatrix :: Matrix
infiniMatrix2 :: Matrix
m31 = Z (LE 1) (Z (LE 1) (LZ (LE 1)))
m22 = Z (E 1 (LE 1)) (LZ (E 1 (LE 1)))
m32 = Z (E 1 (LE 1)) (Z (E 1 (LE 1)) (LZ (E 1 (LE 1))))
m32' = Z (E 1 (LE 1)) (Z (E 1 (LE 1)) (LZ (E 1 (LE 1))))
m11 = LZ (LE 1)
m11' = LZ (LE 1)
m14 = LZ (E 1 (E 1 (E 1 (LE 1))))
m14' = LZ (E 1 (E 1 (E 1 (LE 1))))
m24 = Z (E 1 (E 1 (E 1 (LE 1)))) (LZ (E 1 (E 1 (E 1 (LE 1)))))
m24' = Z (E 1 (E 1 (E 1 (LE 1)))) (LZ (E 1 (E 1 (E 1 (LE 1)))))
n32 = Z (E 2 (LE (-2))) (Z (E 1 (LE (-1))) (LZ (E (-10) (LE 1))))
n32' = Z (E 1 (LE 1)) (Z (E 1 (LE 3)) (LZ (E 1 (LE 1))))
n11 = LZ (LE 5)
n11' = LZ (LE 10)
n14 = LZ (E 1 (E 1 (E 1 (LE 21))))
n14' = LZ (E 1 (E 1 (E 2 (LE 1))))
n24 = Z (E 1 (E 1 (E 1 (LE 1)))) (LZ (E 3 (E 1 (E 1 (LE 1)))))
n24' = Z (E (-11) (E (-99) (E 2321 (LE (-1231))))) (LZ (E (-11) (E 1231 (E (-8888) (LE (-2))))))
mk1 = Z (E 2 (LE 1)) (LZ (LE 10))
mk2 = Z (E 2 (LE 1)) (LZ (E 10 (E 10 (E 2 (LE 2)))))
mk3 = Z (E 1 (E 1 (LE 1))) (LZ (E 1 (E 1 (E 1 (LE 1)))))

infiniRow = E 0 infiniRow

infiniMatrix = Z (LE 1) (Z (E 2 (LE 3)) (Z infiniRow (Z (E (-3) (LE (-2))) (LZ (LE (-1))))))

infiniMatrix2 = Z (LE 1) (Z infiniRow (Z infiniRow (Z infiniRow (LZ (LE (-1))))))

mkz :: Int -> Int -> Zeile
mkz x 1 = LE x
mkz x n = E (n * x) (mkz x (n - 1))

mkm :: Int -> Int -> Int -> Matrix
mkm x 1 n = LZ (mkz x n)
mkm x m n = Z (mkz x n) (mkm x (m - 1) n)

-- qcProps :: TestTree
-- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , QC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]
matrixtypTest :: TestTree
matrixtypTest =
  testGroup
    "matrixtyp"
    [ testCase "keineMatrix 1" $
        matrixtyp mk1 @?= KeineMatrix,
      testCase "keineMatrix 2" $
        matrixtyp mk2 @?= KeineMatrix,
      testCase "keineMatrix 3" $
        matrixtyp mk3 @?= KeineMatrix,
      testCase "(3,2)" $
        matrixtyp m32 @?= Matrix_vom_Typ (3, 2),
      testCase "(1,1)" $
        matrixtyp n11 @?= Matrix_vom_Typ (1, 1),
      testCase "(2,4)" $
        matrixtyp n24 @?= Matrix_vom_Typ (2, 4)
    ]

eqTest :: TestTree
eqTest =
  testGroup
    "equality"
    [ testCase "(==),True" $
        m32 == m32' @?= True,
      testCase "(==),False" $
        m11 == n11 @?= False,
      testCase "(==),fromInteger(), True" $
        1 == LZ (LE 1) @?= True,
      testCase "(/=),True" $
        m32 /= n32 @?= True,
      testCase "(/=),True" $
        n14 /= n14' @?= True,
      testCase "(/=),fromInteger(), True" $
        1 /= LZ (LE 0) @?= True
    ]

plusTest :: TestTree
plusTest =
  testGroup
    "+ Test"
    [ testCase "1" $
        n11 + n11'  @?= LZ (LE 15),
      testCase "2" $
        n32 + n32   @?= Z (E 4 (LE (-4))) (Z (E 2 (LE (-2))) (LZ (E (-20) (LE 2)))),
      testCase "3" $
        m32 + n32   @?= Z (E 3 (LE (-1))) (Z (E 2 (LE 0)) (LZ (E (-9) (LE 2))))
    ]

minusTest :: TestTree
minusTest =
  testGroup
    "- Test"
    [ testCase "1" $
        n11 - n11'  @?= LZ (LE (-5)),
      testCase "2" $
        n32 - n32   @?= Z (E 0 (LE 0)) (Z (E 0 (LE 0)) (LZ (E 0 (LE 0)))),
      testCase "3" $
        m32 - n32   @?= Z (E (-1) (LE 3)) (Z (E 0 (LE 2)) (LZ (E 11 (LE 0))))
    ]

absTest :: TestTree
absTest =
  testGroup
    "abs Test"
    [ testCase "1" $
        abs n11'  @?= LZ (LE 10),
      testCase "2" $
        abs n32   @?= Z (E 2 (LE 2)) (Z (E 1 (LE 1)) (LZ (E 10 (LE 1)))),
      testCase "3" $
        abs n24'  @?= Z (E 11 (E 99 (E 2321 (LE 1231)))) (LZ (E 11 (E 1231 (E 8888 (LE 2)))))
    ]

mixedTest :: TestTree
mixedTest =
  testGroup
    "mixed Test"
    [ testCase "1" $
        n32 + abs n32      @?= Z (E 4 (LE 0)) (Z (E 2 (LE 0)) (LZ (E 0 (LE 2)))),
      testCase "2" $
        abs m24 - abs n24' @?= Z (E (-10) (E (-98) (E (-2320) (LE (-1230))))) (LZ (E (-10) (E (-1230) (E (-8887) (LE (-1)))))),
      testCase "3" $
        n24' - abs n24'    @?= Z (E (-22) (E (-198) (E 0 (LE (-2462))))) (LZ (E (-22) (E 0 (E (-17776) (LE (-4))))))
    ]

{-
errorTest :: TestTree
errorTest =
  testGroup
    "these should fail with \"Argument(e) typfehlerhaft\"\n there are 2 more test groups on matrices with infinite length columns to uncomment at your own discretion (line 17,18) \n "
    [ testCase "(+) (exception desired)" $ assertError "Argument(e) typfehlerhaft" (m14 + n11),
      testCase "(-) (exception desired)" $ assertError "Argument(e) typfehlerhaft" (abs m32 - n24),
      testCase "(==)(exception desired)" $ assertError "Argument(e) typfehlerhaft" (n24' == m32),
      testCase "(==)(exception desired)" $ assertError "Argument(e) typfehlerhaft" (m31 == mk1),
      testCase "(/=)(exception desired)" $ assertError "Argument(e) typfehlerhaft" (mk1 /= m31)
    ]

infiniteTest1 :: TestTree
infiniteTest1 =
  testGroup
    "row 3/5 is infinite"
    [ testCase "matrixtyp (succeeds or recurses infinitely)" $
        matrixtyp infiniMatrix @?= KeineMatrix,
      testCase "(+) (exception desired)" $ assertError "Argument(e) typfehlerhaft" (infiniMatrix + m32),
      testCase "(-) (exception desired)" $ assertError "Argument(e) typfehlerhaft" (n24' - infiniMatrix),
      testCase "abs (exception desired)" $ assertError "Argument(e) typfehlerhaft" (abs infiniMatrix),
      testCase "(==)(exception desired)" $ assertError "Argument(e) typfehlerhaft" (infiniMatrix == infiniMatrix),
      testCase "(/=)(exception desired)" $ assertError "Argument(e) typfehlerhaft" (infiniMatrix /= infiniMatrix)
    ]

infiniteTest2 :: TestTree
infiniteTest2 =
  testGroup
    "rows 1 and 5 are safe - 2,3,4 are infinite"
    [ testCase "matrixtyp (succeeds or recurses infinitely)" $ matrixtyp infiniMatrix2 @?= KeineMatrix,
      testCase "(+) (exception desired)" $ assertError "Argument(e) typfehlerhaft" (infiniMatrix2 + m32),
      testCase "(-) (exception desired)" $ assertError "Argument(e) typfehlerhaft" (n24' - infiniMatrix2),
      testCase "abs (exception desired)" $ assertError "Argument(e) typfehlerhaft" (abs infiniMatrix2),
      testCase "(==)(exception desired)" $ assertError "Arguent(e) typfehlerhaft" (infiniMatrix2 == infiniMatrix2),
      testCase "(/=)(exception desired)" $ assertError "Argument(e) typfehlerhaft" (infiniMatrix2 /= infiniMatrix2)
    ]

stressTest :: TestTree
stressTest =
  testGroup
    "stress tests"
    [ testCase "matrixtyp = (10000, 10000)" $
        matrixtyp (mkm 0 10000 10000) @?= Matrix_vom_Typ (10000, 10000),
      testCase "(==),True, (1000,1000)" $
        mkm 0 1000 1000 == mkm 0 1000 1000 @?= True,
      testCase "(/=),same instance, instant if based on (==)" $
        mkm 0 1000 1000 /= mkm 0 1000 1000 @?= False,
      testCase "(+), (1000,1000)" $
        mkm 3 1000 1000 + mkm 2 1000 1000 @?= mkm 5 1000 1000,
      testCase "(-), (1000,1000)" $
        mkm 3 1000 1000 - mkm 1 1000 1000 @?= mkm 2 1000 1000,
      testCase "abs, (1000,1000)" $
        abs (mkm (-3) 1000 1000) @?= mkm 3 1000 1000,
      testCase "mixed, (1000,1000)" $
        mkm 3 1000 1000 - abs (mkm (-2) 1000 1000) + mkm 4 1000 1000 @?= mkm 5 1000 1000,
      testCase "(/=) mismatched matrices (exception desired)" $
        assertError "Argument(e) typfehlerhaft" (mkm 0 10000 10000 /= mkm 0 10000 10001)
    ]
 -}
