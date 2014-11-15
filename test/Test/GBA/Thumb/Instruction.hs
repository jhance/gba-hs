{-# LANGUAGE QuasiQuotes #-}
module Test.GBA.Thumb.Instruction
    (tests)
where

import           Language.Literals.Binary

import           Game.GBA.Thumb.Instruction

import           Test.HUnit
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

parseTests :: TestTree
parseTests = testGroup "parser" $
    [ testGroup "shift register (t1)" $
        [ shiftReg1
        , shiftReg2
        , shiftReg3
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
    ]

-- | Eventually this will go in Test.GBA.Thumb
tests :: TestTree
tests = testGroup "thumb instructions" $
            [
            parseTests
            ]

--- t1
------
shiftReg1 :: TestTree
shiftReg1 = testCase "shift regester LSL" $ parseT [b|000 00 00101 010 111|]
    @?= TSR TSRO_LSL [b|00101|] [b|010|] [b|111|]

shiftReg2 :: TestTree
shiftReg2 = testCase "shift register LSR" $ parseT [b|000 01 11010 000 001|]
    @?= TSR TSRO_LSR [b|11010|] [b|000|] [b|001|]

shiftReg3 :: TestTree
shiftReg3 = testCase "shift register ASR" $ parseT [b|000 10 11111 101 000|]
    @?= TSR TSRO_ASR [b|11111|] [b|101|] [b|000|]

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

tmcas4 :: TestTree
tmcas4 = testCase "subtract immediate" $ parseT [b|001 11 111 10000101|]
    @?= TMCAS TMCASO_SUB [b|111|] [b|10000101|]