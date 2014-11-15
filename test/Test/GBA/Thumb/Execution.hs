{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
module Test.GBA.Thumb.Execution
    (tests)
where

import           Control.Applicative
import           Control.Monad.ST
import           Language.Literals.Binary

import           Data.Bits
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
      , testGroup "asr (t1.3)" $
          [ shiftRegASR1
          , shiftRegASR2
          , shiftRegASR3
          , shiftRegASR4
          ]
      ]
    , testGroup "add/subtract (t2)" $
      [ testGroup "add (t2.1)" $
          [ tasAdd1
          , tasAdd2
          , tasAdd3
          , tasAdd4
          ]
      , testGroup "subtract (t2.2)" $
          [tasSub1
          --, tasSub2
          ]
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
shiftRegLSR1 = testCase "shift register LSR by 1" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|010|] $ 2^20
        execute (TSR TSRO_LSR 1 [b|010|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^19
    c @?= False
    z @?= False
    n @?= False

shiftRegLSR2 :: TestTree
shiftRegLSR2 = testCase "shift register LSR by 8" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|010|] $ 2^20
        execute (TSR TSRO_LSR 8 [b|010|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^12
    c @?= False
    z @?= False
    n @?= False

shiftRegLSR3 :: TestTree
shiftRegLSR3 = testCase "shift register LSR with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20 + 2
        execute (TSR TSRO_LSR 2 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^18
    c @?= True
    z @?= False
    n @?= False

shiftRegLSR4 :: TestTree
shiftRegLSR4 = testCase "shift register LSR to zero" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (TSR TSRO_LSR 30 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= False
    z @?= True
    n @?= False

shiftRegLSR5 :: TestTree
shiftRegLSR5 = testCase "shift register LSR to zero with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (TSR TSRO_LSR 21 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= True
    z @?= True
    n @?= False

-- t1.3
-------
shiftRegASR1 :: TestTree
shiftRegASR1 = testCase "shift register ASR by 1, no sign bit" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (TSR TSRO_ASR 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^19
    c @?= False
    z @?= False
    n @?= False

shiftRegASR2 :: TestTree
shiftRegASR2 = testCase "shift register ASR by 1, with sign bit" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20 + 2^31
        execute (TSR TSRO_ASR 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^19 + 2^31 + 2^30
    c @?= False
    z @?= False
    n @?= True

shiftRegASR3 :: TestTree
shiftRegASR3 = testCase "shift register ASR to zero with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (TSR TSRO_ASR 21 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= True
    z @?= True
    n @?= False

shiftRegASR4 :: TestTree
shiftRegASR4 = testCase "shift register ASR with sign bit multiple, with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20 + 2^31 + 2^4
        execute (TSR TSRO_ASR 5 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^15 + 2^31 + 2^30 + 2^29 + 2^28 + 2^27 + 2^26
    c @?= True
    z @?= False
    n @?= True

-- t3
-----

getTASResult :: RegisterID -> GBA s (Word32, Bool, Bool, Bool, Bool)
getTASResult reg = (,,,,)
    <$> readRegister reg
    <*> getCondition CFCarry
    <*> getCondition CFZero
    <*> getCondition CFSign
    <*> getCondition CFOverflow

tasAdd1 :: TestTree
tasAdd1 = testCase "tas add simple immediate" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] $ 5
        execute (TAS TASO_NUM TASO_ADD 4 [b|000|] [b|001|])
        getTASResult [b|001|]
    dest @?= 9
    c @?= False
    z @?= False
    n @?= False
    v @?= False

tasAdd2 :: TestTree
tasAdd2 = testCase "tas add simple register" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] $ 5
        writeRegister [b|010|] $ 4
        execute (TAS TASO_REG TASO_ADD [b|010|] [b|000|] [b|001|])
        getTASResult [b|001|]
    dest @?= 9
    c @?= False
    z @?= False
    n @?= False
    v @?= False

tasAdd3 :: TestTree
tasAdd3 = testCase "tas add simple register, overflow/zero" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] $ 2^31
        writeRegister [b|010|] $ 2^31
        execute (TAS TASO_REG TASO_ADD [b|010|] [b|000|] [b|001|])
        getTASResult [b|001|]
    dest @?= 0
    c @?= True
    z @?= True
    n @?= False
    v @?= True

tasAdd4 :: TestTree
tasAdd4 = testCase "tas add simple register, nonsigned overflow" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] $ complement 0
        writeRegister [b|010|] $ complement 0 - 2^31
        execute (TAS TASO_REG TASO_ADD [b|010|] [b|000|] [b|001|])
        getTASResult [b|001|]
    --dest @?= 0
    c @?= True
    z @?= False
    n @?= False
    v @?= False

tasSub1 :: TestTree
tasSub1 = testCase "tas sub simple register" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] $ 5
        writeRegister [b|010|] $ 3
        execute (TAS TASO_REG TASO_SUB [b|010|] [b|000|] [b|001|])
        getTASResult [b|001|]
    dest @?= 2
    c @?= True -- Is this right?!
    z @?= False
    n @?= False
    v @?= False