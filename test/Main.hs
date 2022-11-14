module Main where

import Test.Tasty

-- More examples available at: https://github.com/UnkindPartition/tasty
-- Run these via either `cabal test`
-- or `stack test`

-- import qualified TestSuite1
-- import qualified Angabe1_TestSuite
-- import qualified TestSuite2
-- import qualified Angabe2_TestSuite
-- import qualified TestSuite3
-- import qualified Angabe3_TestSuite
--import qualified TestSuite4
--import qualified Angabe4_TestSuite
import qualified TestSuite5
--import qualified Angabe5_TestSuite
--import qualified TestSuite6
--import qualified Angabe6_TestSuite
--import qualified TestSuite7
--import qualified Angabe7_TestSuite

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Fprog Tests"
    [ 
    --  TestSuite1.spec
    -- ,Angabe1_TestSuite.spec
    -- ,TestSuite2.spec
    -- ,Angabe2_TestSuite.spec
    -- ,TestSuite3.spec
    -- ,Angabe3_TestSuite.spec
    -- ,TestSuite4.spec
    -- ,Angabe4_TestSuite.spec
    TestSuite5.spec
    -- ,Angabe5_TestSuite.spec
    -- ,TestSuite6.spec
    -- ,Angabe6_TestSuite.spec
    -- ,TestSuite7.spec
    -- ,Angabe7_TestSuite.spec
    ]
