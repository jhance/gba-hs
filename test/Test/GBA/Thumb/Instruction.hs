{-# LANGUAGE QuasiQuotes #-}
module Test.GBA.Thumb.Instruction
    (tests)
where

import           Language.Literals.Binary

import           Game.GBA.Thumb.Instruction

import           Test.HUnit
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testGroup "parser" $
    [ testGroup "shift register (t1)" $
        [ shiftReg1
        , shiftReg2
        , shiftReg3
        , shiftReg4
        , shiftReg5
        , shiftReg6
        ]
    , testGroup "add/subtract (t2)"
        [ tas1
        , tas2
        , tas3
        , tas4
        ]
    , testGroup "move/compare/add/subtract immediate (t3)"
        [ tmcas1
        , tmcas2
        , tmcas3
        , tmcas4
        ]
    , testGroup "alu operations (t4)"
        [ talu1
        , talu2
        , talu3
        , talu4
        , talu5
        , talu6
        , talu7
        , talu8
        ]
    ]

--- t1
------
shiftReg1 :: TestTree
shiftReg1 = testCase "shift register LSL" $ parseT [b|000 00 00101 010 111|]
    @?= TSR TSRO_LSL [b|00101|] [b|010|] [b|111|]

shiftReg2 :: TestTree
shiftReg2 = testCase "shift register LSR" $ parseT [b|000 01 11010 000 001|]
    @?= TSR TSRO_LSR [b|11010|] [b|000|] [b|001|]

shiftReg3 :: TestTree
shiftReg3 = testCase "shift register ASR" $ parseT [b|000 10 11111 101 000|]
    @?= TSR TSRO_ASR [b|11111|] [b|101|] [b|000|]

shiftReg4 :: TestTree
shiftReg4 = testCase "shift register LSL 0 -> 0" $ parseT [b|000 00 00000 010 111|]
    @?= TSR TSRO_LSL [b|00000|] [b|010|] [b|111|]

shiftReg5 :: TestTree
shiftReg5 = testCase "shift register LSR 0 -> 32" $ parseT [b|000 01 00000 000 001|]
    @?= TSR TSRO_LSR [b|100000|] [b|000|] [b|001|]

shiftReg6 :: TestTree
shiftReg6 = testCase "shift register ASR 0 -> 32" $ parseT [b|000 10 00000 101 000|]
    @?= TSR TSRO_ASR [b|100000|] [b|101|] [b|000|]

--- t2
------
tas1 :: TestTree
tas1 = testCase "add register" $ parseT [b|00011 00 010 111 001|]
    @?= TAS TASO_REG TASO_ADD [b|010|] [b|111|] [b|001|]

tas2 :: TestTree
tas2 = testCase "subtract register" $ parseT [b|00011 01 111 000 101|]
    @?= TAS TASO_REG TASO_SUB [b|111|] [b|000|] [b|101|]

tas3 :: TestTree
tas3 = testCase "add immediate" $ parseT [b|00011 10 001 000 010|]
    @?= TAS TASO_NUM TASO_ADD [b|001|] [b|000|] [b|010|]

tas4 :: TestTree
tas4 = testCase "subtract immediate" $ parseT [b|00011 11 010 111 011|]
    @?= TAS TASO_NUM TASO_SUB [b|010|] [b|111|] [b|011|]

--- t3
------
tmcas1 :: TestTree
tmcas1 = testCase "move immediate" $ parseT [b|001 00 010 00000011|]
    @?= TMCAS TMCASO_MOV [b|010|] [b|00000011|]

tmcas2 :: TestTree
tmcas2 = testCase "cmp immediate" $ parseT [b|001 01 011 11111111|]
    @?= TMCAS TMCASO_CMP [b|011|] [b|11111111|]

tmcas3 :: TestTree
tmcas3 = testCase "add immediate" $ parseT [b|001 10 000 10101010|]
    @?= TMCAS TMCASO_ADD [b|000|] [b|10101010|]

-- | Subtract is just add with a twos-complement so its not actually
-- that different.
tmcas4 :: TestTree
tmcas4 = testCase "subtract immediate" $ parseT [b|001 11 111 10000101|]
    @?= TMCAS TMCASO_SUB [b|111|] [b|10000101|]

--- t4
------
talu1 :: TestTree
talu1 = testCase "and" $ parseT [b|010000 0000 010 100|]
    @?= TALU TALU_AND [b|010|] [b|100|]

talu2 :: TestTree
talu2 = testCase "eor" $ parseT [b|010000 0001 010 100|]
    @?= TALU TALU_EOR [b|010|] [b|100|]

talu3 :: TestTree
talu3 = testCase "lsl" $ parseT [b|010000 0010 010 100|]
    @?= TALU TALU_LSL [b|010|] [b|100|]

talu4 :: TestTree
talu4 = testCase "lsr" $ parseT [b|010000 0011 010 100|]
    @?= TALU TALU_LSR [b|010|] [b|100|]

talu5 :: TestTree
talu5 = testCase "asr" $ parseT [b|010000 0100 011 101|]
    @?= TALU TALU_ASR [b|011|] [b|101|]

talu6 :: TestTree
talu6 = testCase "adc" $ parseT [b|010000 0101 011 101|]
    @?= TALU TALU_ADC [b|011|] [b|101|]

talu7 :: TestTree
talu7 = testCase "sbc" $ parseT [b|010000 0110 011 101|]
    @?= TALU TALU_SBC [b|011|] [b|101|]

talu8 :: TestTree
talu8 = testCase "ror" $ parseT [b|010000 0111 011 101|]
    @?= TALU TALU_ROR [b|011|] [b|101|]