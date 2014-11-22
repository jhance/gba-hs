{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.GBA.Thumb.T4
    (tests)
where

import Control.Applicative
import Data.Bits
import Data.Word
import Language.Literals.Binary

import Test.HUnit
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Modifiers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import Game.GBA.Monad
import Game.GBA.Register
import Game.GBA.Thumb.Execution
import Game.GBA.Thumb.Instruction

import Test.GBA.Common

tests :: TestTree
tests = testGroup "[t4] alu operations"
    [ testGroup "[t4.0] parser"
        [ t4parser1
        , t4parser2
        , t4parser3
        , t4parser4
        , t4parser5
        , t4parser6
        , t4parser7
        , t4parser8
        , t4parser9
        , t4parser10
        , t4parser11
        , t4parser12
        , t4parser13
        , t4parser14
        , t4parser15
        , t4parser16
        ]
    , testGroup "[t4.1] and"
        [ t4and1
        , t4and2
        , t4and3
        , t4and4
        , t4and5
        , t4and6
        ]
    ]

t4parser1 :: TestTree
t4parser1 = testCase "and" $ parseT [b|010000 0000 010 100|]
    @?= T4 T4_AND [b|010|] [b|100|]

t4parser2 :: TestTree
t4parser2 = testCase "eor" $ parseT [b|010000 0001 010 100|]
    @?= T4 T4_EOR [b|010|] [b|100|]

t4parser3 :: TestTree
t4parser3 = testCase "lsl" $ parseT [b|010000 0010 010 100|]
    @?= T4 T4_LSL [b|010|] [b|100|]

t4parser4 :: TestTree
t4parser4 = testCase "lsr" $ parseT [b|010000 0011 010 100|]
    @?= T4 T4_LSR [b|010|] [b|100|]

t4parser5 :: TestTree
t4parser5 = testCase "asr" $ parseT [b|010000 0100 011 101|]
    @?= T4 T4_ASR [b|011|] [b|101|]

t4parser6 :: TestTree
t4parser6 = testCase "adc" $ parseT [b|010000 0101 011 101|]
    @?= T4 T4_ADC [b|011|] [b|101|]

t4parser7 :: TestTree
t4parser7 = testCase "sbc" $ parseT [b|010000 0110 011 101|]
    @?= T4 T4_SBC [b|011|] [b|101|]

t4parser8 :: TestTree
t4parser8 = testCase "ror" $ parseT [b|010000 0111 011 101|]
    @?= T4 T4_ROR [b|011|] [b|101|]

t4parser9 :: TestTree
t4parser9 = testCase "tst" $ parseT [b|010000 1000 011 101|]
    @?= T4 T4_TST [b|011|] [b|101|]

t4parser10 :: TestTree
t4parser10 = testCase "neg" $ parseT [b|010000 1001 011 101|]
    @?= T4 T4_NEG [b|011|] [b|101|]

t4parser11 :: TestTree
t4parser11 = testCase "sbc" $ parseT [b|010000 1010 011 101|]
    @?= T4 T4_CMP [b|011|] [b|101|]

t4parser12 :: TestTree
t4parser12 = testCase "ror" $ parseT [b|010000 1011 011 101|]
    @?= T4 T4_CMN [b|011|] [b|101|]

t4parser13 :: TestTree
t4parser13 = testCase "orr" $ parseT [b|010000 1100 011 101|]
    @?= T4 T4_ORR [b|011|] [b|101|]

t4parser14 :: TestTree
t4parser14 = testCase "mul" $ parseT [b|010000 1101 011 101|]
    @?= T4 T4_MUL [b|011|] [b|101|]

t4parser15 :: TestTree
t4parser15 = testCase "bic" $ parseT [b|010000 1110 011 101|]
    @?= T4 T4_BIC [b|011|] [b|101|]

t4parser16 :: TestTree
t4parser16 = testCase "mvn" $ parseT [b|010000 1111 011 101|]
    @?= T4 T4_MVN [b|011|] [b|101|]

-- t4.1
-------
t4and1 :: TestTree
t4and1 = testProperty "and correctly sets register (src /= dest)" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_AND src dest
        result <- readSafeRegister dest
        return $ result == n1 .&. n2

t4and2 :: TestTree
t4and2 = testProperty "and correctly sets register (src == dest)" $
    \(n, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r n
        execute $ T4 T4_AND r r
        result <- readSafeRegister r
        return $ result == n

t4and3 :: TestTree
t4and3 = testProperty "and Z flag" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_AND src dest
        result <- readSafeRegister dest
        z <- readStatus statusZ
        return $ z == (result == 0)

t4and4 :: TestTree
t4and4 = testProperty "and Z flag set with zero src" $
    \(n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        let n1 = 0
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_AND src dest
        result <- readSafeRegister dest
        readStatus statusZ

t4and5 :: TestTree
t4and5 = testProperty "and Z flag set with zero dest" $
    \(n1, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        let n2 = 0
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_AND src dest
        result <- readSafeRegister dest
        readStatus statusZ

t4and6 :: TestTree
t4and6 = testProperty "and N flag" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_AND src dest
        n <- readStatus statusN
        return $ n == testBit (getLarge n1 .&. getLarge n2) 31