{-# LANGUAGE QuasiQuotes #-}
module Test.GBA.Thumb.T1
    (tests)
where

import Control.Applicative
import Data.Word
import Language.Literals.Binary

import Test.HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Game.GBA.Monad
import Game.GBA.Register
import Game.GBA.Thumb.Execution
import Game.GBA.Thumb.Instruction

import Test.GBA.Common

getShiftResult :: RegisterID -> GBA s (Word32, Bool, Bool, Bool)
getShiftResult reg = (,,,)
    <$> readRegister reg
    <*> readStatus statusC
    <*> readStatus statusZ
    <*> readStatus statusN

tests :: TestTree
tests = testGroup "[t1] shift register"
    [ testGroup "[t1.0] parser"
        [ shiftRegParse1
        , shiftRegParse2
        , shiftRegParse3
        , shiftRegParse4
        , shiftRegParse5
        , shiftRegParse6
        ]
    , testGroup "[t1.1] lsl"
        [ shiftRegLSL1
        , shiftRegLSL2
        , shiftRegLSL3
        , shiftRegLSL4
        , shiftRegLSL5
        , shiftRegLSL6
        , shiftRegLSL7
        , shiftRegLSL8
        ]
    , testGroup "[t1.2] lsr"
        [ shiftRegLSR1
        , shiftRegLSR2
        , shiftRegLSR3
        , shiftRegLSR4
        , shiftRegLSR5
        ]
    , testGroup "[t1.3] asr"
       [ shiftRegASR1
        , shiftRegASR2
        , shiftRegASR3
        , shiftRegASR4
        ]
    ]

shiftRegParse1 :: TestTree
shiftRegParse1 = testCase "shift register lsl" $ parseT [b|000 00 00101 010 111|]
    @?= T1 T1_LSL [b|00101|] [b|010|] [b|111|]

shiftRegParse2 :: TestTree
shiftRegParse2 = testCase "shift register lsr" $ parseT [b|000 01 11010 000 001|]
    @?= T1 T1_LSR [b|11010|] [b|000|] [b|001|]

shiftRegParse3 :: TestTree
shiftRegParse3 = testCase "shift register asr" $ parseT [b|000 10 11111 101 000|]
    @?= T1 T1_ASR [b|11111|] [b|101|] [b|000|]

shiftRegParse4 :: TestTree
shiftRegParse4 = testCase "shift register lsl 0 -> 0" $ parseT [b|000 00 00000 010 111|]
    @?= T1 T1_LSL [b|00000|] [b|010|] [b|111|]

shiftRegParse5 :: TestTree
shiftRegParse5 = testCase "shift register lsr 0 -> 32" $ parseT [b|000 01 00000 000 001|]
    @?= T1 T1_LSR [b|100000|] [b|000|] [b|001|]

shiftRegParse6 :: TestTree
shiftRegParse6 = testCase "shift register asr 0 -> 32" $ parseT [b|000 10 00000 101 000|]
    @?= T1 T1_ASR [b|100000|] [b|101|] [b|000|]

-- t1.1
-------
shiftRegLSL1 :: TestTree
shiftRegLSL1 = testCase "shift register lsl by 0" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|010|] 52
        execute (T1 T1_LSL 0 [b|010|] [b|100|])
        getShiftResult [b|100|]
    dest @?= 52
    c @?= False
    z @?= False
    n @?= False

shiftRegLSL2 :: TestTree
shiftRegLSL2 = testCase "shift register lsl by 0 (with zero contents)" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|010|] 0
        execute (T1 T1_LSL 0 [b|010|] [b|100|])
        getShiftResult [b|100|]
    dest @?= 0
    c @?= False
    z @?= True
    n @?= False

shiftRegLSL3 :: TestTree
shiftRegLSL3 = testCase "shift register lsl by 1" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] 52
        execute (T1 T1_LSL 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 104
    c @?= False
    z @?= False
    n @?= False

shiftRegLSL4 :: TestTree
shiftRegLSL4 = testCase "shift register lsl by 1 with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 52 + 2^31
        execute (T1 T1_LSL 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 104
    c @?= True
    z @?= False
    n @?= False

shiftRegLSL5 :: TestTree
shiftRegLSL5 = testCase "shift register lsl by 1 to zero without carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 0
        execute (T1 T1_LSL 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= False
    z @?= True
    n @?= False

shiftRegLSL6 :: TestTree
shiftRegLSL6 = testCase "shift register lsl by 1 to zero with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^31
        execute (T1 T1_LSL 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= True
    z @?= True
    n @?= False

shiftRegLSL7 :: TestTree
shiftRegLSL7 = testCase "shift register lsl by 1 with sign" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^30 + 2^31
        execute (T1 T1_LSL 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^31
    c @?= True
    z @?= False
    n @?= True

shiftRegLSL8 :: TestTree
shiftRegLSL8 = testCase "shift register lsl by 8" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (T1 T1_LSL 8 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^28
    c @?= False
    z @?= False
    n @?= False

-- t1.2
-------
-- invariant: n will always be false.
shiftRegLSR1 :: TestTree
shiftRegLSR1 = testCase "shift register lsr by 1" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|010|] $ 2^20
        execute (T1 T1_LSR 1 [b|010|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^19
    c @?= False
    z @?= False
    n @?= False

shiftRegLSR2 :: TestTree
shiftRegLSR2 = testCase "shift register lsr by 8" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|010|] $ 2^20
        execute (T1 T1_LSR 8 [b|010|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^12
    c @?= False
    z @?= False
    n @?= False

shiftRegLSR3 :: TestTree
shiftRegLSR3 = testCase "shift register lsr with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20 + 2
        execute (T1 T1_LSR 2 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^18
    c @?= True
    z @?= False
    n @?= False

shiftRegLSR4 :: TestTree
shiftRegLSR4 = testCase "shift register lsr to zero" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (T1 T1_LSR 30 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= False
    z @?= True
    n @?= False

shiftRegLSR5 :: TestTree
shiftRegLSR5 = testCase "shift register lsr to zero with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (T1 T1_LSR 21 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= True
    z @?= True
    n @?= False

-- t1.3
-------
shiftRegASR1 :: TestTree
shiftRegASR1 = testCase "shift register asr by 1, no sign bit" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (T1 T1_ASR 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^19
    c @?= False
    z @?= False
    n @?= False

shiftRegASR2 :: TestTree
shiftRegASR2 = testCase "shift register asr by 1, with sign bit" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20 + 2^31
        execute (T1 T1_ASR 1 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^19 + 2^31 + 2^30
    c @?= False
    z @?= False
    n @?= True

shiftRegASR3 :: TestTree
shiftRegASR3 = testCase "shift register asr to zero with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20
        execute (T1 T1_ASR 21 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 0
    c @?= True
    z @?= True
    n @?= False

shiftRegASR4 :: TestTree
shiftRegASR4 = testCase "shift register asr with sign bit multiple, with carry" $ do
    (dest, c, z, n) <- runTest $ do
        writeRegister [b|000|] $ 2^20 + 2^31 + 2^4
        execute (T1 T1_ASR 5 [b|000|] [b|001|])
        getShiftResult [b|001|]
    dest @?= 2^15 + 2^31 + 2^30 + 2^29 + 2^28 + 2^27 + 2^26
    c @?= True
    z @?= False
    n @?= True
