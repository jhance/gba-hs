{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
module Test.GBA.Thumb.Execution
    (tests)
where

import           Control.Applicative
import           Control.Monad.ST
import           Language.Literals.Binary

import           Data.Word

import           Game.GBA.Boot (bootForTest)
import           Game.GBA.CPUFlag
import           Game.GBA.Monad
import           Game.GBA.Register
import           Game.GBA.Thumb.Instruction
import           Game.GBA.Thumb.Execution

import           Test.HUnit
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

runTest :: (forall s. GBA s a) -> IO a
runTest gba = stToIO $ do
    context <- makeGBAContext
    runGBA context (bootForTest Thumb >> gba)

tests :: TestTree
tests = testGroup "execution"
    [ testGroup "shift register (t1)" $
      [ testGroup "lsl (t1.1)" $ 
          [ shiftRegLSL1
          , shiftRegLSL2
          , shiftRegLSL3
          , shiftRegLSL4
          , shiftRegLSL5
          , shiftRegLSL6
          , shiftRegLSL7
          , shiftRegLSL8
          ]
      , testGroup "lsr (t1.2)" $
          [ shiftRegLSR1
          , shiftRegLSR2
          , shiftRegLSR3
          , shiftRegLSR4
          , shiftRegLSR5
          ]
      , testGroup "asr (t1.3)" []
      ]
    ]

-- t1
-----
getShiftResult :: RegisterID -> GBA s (Word32, Bool, Bool, Bool)
getShiftResult reg = (,,,)
    <$> readRegister reg
    <*> getCondition CFCarry
    <*> getCondition CFZero
    <*> getCondition CFSign

-- t1.1
-------
shiftRegLSL1 :: TestTree
shiftRegLSL1 = testCase "shift register LSL by 0" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|010|] 52
        execute (TSR TSRO_LSL 0 [b|010|] [b|100|])
        getShiftResult [b|100|]
    dest @?= 52
    c @?= False
    z @?= False
    n @?= False

shiftRegLSL2 :: TestTree
shiftRegLSL2 = testCase "shift register LSL by 0 (with zero contents)" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|010|] 0
        execute (TSR TSRO_LSL 0 [b|010|] [b|100|])
        getShiftResult [b|100|]
    dest @?= 0
    c @?= False
    z @?= True
    n @?= False

shiftRegLSL3 :: TestTree
shiftRegLSL3 = testCase "shift register LSL by 1" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] 52
        execute (TSR TSRO_LSL 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 104
    c @?= False
    z @?= False
    n @?= False

shiftRegLSL4 :: TestTree
shiftRegLSL4 = testCase "shift register LSL by 1 with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 52 + 2^31
        execute (TSR TSRO_LSL 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 104
    c @?= True
    z @?= False
    n @?= False

shiftRegLSL5 :: TestTree
shiftRegLSL5 = testCase "shift register LSL by 1 to zero without carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 0
        execute (TSR TSRO_LSL 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= False
    z @?= True
    n @?= False

shiftRegLSL6 :: TestTree
shiftRegLSL6 = testCase "shift register LSL by 1 to zero with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^31
        execute (TSR TSRO_LSL 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= True
    z @?= True
    n @?= False

shiftRegLSL7 :: TestTree
shiftRegLSL7 = testCase "shift register LSL by 1 with sign" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^30 + 2^31
        execute (TSR TSRO_LSL 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^31
    c @?= True
    z @?= False
    n @?= True

shiftRegLSL8 :: TestTree
shiftRegLSL8 = testCase "shift register LSL by 8" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (TSR TSRO_LSL 8 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^28
    c @?= False
    z @?= False
    n @?= False

-- t1.2
-------
-- invariant: n will always be false.
shiftRegLSR1 :: TestTree
shiftRegLSR1 = testCase "shift register RSL by 1" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|010|] $ 2^20
        execute (TSR TSRO_LSR 1 [b|010|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^19
    c @?= False
    z @?= False
    n @?= False

shiftRegLSR2 :: TestTree
shiftRegLSR2 = testCase "shift register RSL by 8" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|010|] $ 2^20
        execute (TSR TSRO_LSR 8 [b|010|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^12
    c @?= False
    z @?= False
    n @?= False

shiftRegLSR3 :: TestTree
shiftRegLSR3 = testCase "shift register RSL with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20 + 2
        execute (TSR TSRO_LSR 2 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^18
    c @?= True
    z @?= False
    n @?= False

shiftRegLSR4 :: TestTree
shiftRegLSR4 = testCase "shift register RSL to zero" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (TSR TSRO_LSR 30 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= False
    z @?= True
    n @?= False

shiftRegLSR5 :: TestTree
shiftRegLSR5 = testCase "shift register RSL to zero with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (TSR TSRO_LSR 21 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= True
    z @?= True
    n @?= False
