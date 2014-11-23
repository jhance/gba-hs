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
    , testGroup "[t4.2] eor"
        [ t4eor1
        , t4eor2
        , t4eor3
        , t4eor4
        ]
    , testGroup "[t4.3] lsl"
        [
        ]
    , testGroup "[t4.4] lsr"
        [
        ]
    , testGroup "[t4.5] asr"
        [
        ]
    , testGroup "[t4.6] adc"
        [ t4adc1

        , t4adc2a
        , t4adc2b
        , t4adc2c
        , t4adc2d

        , t4adc3a
        , t4adc3b
        , t4adc3c

        , t4adc4a
        , t4adc4b

        , t4adc5a
        , t4adc5b
        , t4adc5c

        , t4adc6a
        , t4adc6b
        ]
    , testGroup "[t4.7] sbc"
        [ t4sbc1
        , t4sbc2a
        , t4sbc2b
        , t4sbc2c
        , t4sbc2d
        , t4sbc2e
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

t4eor1 :: TestTree
t4eor1 = testProperty "eor correctly sets register (src /= dest)" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_EOR src dest
        result <- readSafeRegister dest
        return $ result == n1 `xor` n2

t4eor2 :: TestTree
t4eor2 = testProperty "eor correctly sets register (src == dest)" $
    \(n, (ThumbRegister src)) -> runPure $ do
        writeSafeRegister src n
        writeSafeRegister src n
        execute $ T4 T4_EOR src src
        result <- readSafeRegister src
        return $ result == 0

t4eor3 :: TestTree
t4eor3 = testProperty "eor Z flag, set to 1" $
    \(n, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n
        writeSafeRegister dest n
        execute $ T4 T4_EOR src dest
        readStatus statusZ

t4eor4 :: TestTree
t4eor4 = testProperty "eor Z flag, set to 0" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) ->
      src /= dest && n1 /= n2 ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_EOR src dest
        not <$> readStatus statusZ

-- t4.6
t4adc1 :: TestTree
t4adc1 = testProperty "[t4adc1] correctly sets register (src /= dest)" $
    \(n1, n2, c, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        writeStatus statusC c
        execute $ T4 T4_ADC src dest
        result <- readSafeRegister dest
        return $ result == n1 + n2 + fromIntegral (fromEnum c)

t4adc2a :: TestTree
t4adc2a = testCase "[t4adc2a] Z flag, set to 0" $ do
    z <- runTest $ do
        writeSafeRegister 0 4
        writeSafeRegister 1 5
        writeStatus statusC True
        execute $ T4 T4_ADC 0 1
        result <- readSafeRegister 1
        readStatus statusZ
    z @?= False

t4adc2b :: TestTree
t4adc2b = testProperty "[t4adc2b] Z flag, set to 0, with no pre-carry" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) ->
      src /= dest && n1 + n2 /= 0 ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        writeStatus statusC False
        execute $ T4 T4_ADC src dest
        not <$> readStatus statusZ

t4adc2c :: TestTree
t4adc2c = testProperty "[t4adc2c] Z flag, set to 1, with no pre-carry" $
    \(n, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n
        writeSafeRegister dest $ maxBound - n + 1
        writeStatus statusC False
        execute $ T4 T4_ADC src dest
        readStatus statusZ

t4adc2d :: TestTree
t4adc2d = testProperty "[t4adc2d] Z flag, set to 1, with pre-carry" $
    \(n, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n
        writeSafeRegister dest $ maxBound - n
        writeStatus statusC True
        execute $ T4 T4_ADC src dest
        readStatus statusZ

t4adc3a :: TestTree
t4adc3a = testCase "[t4adc3a] C flag, set to 0" $ do
    c <- runTest $ do
        writeSafeRegister 0 [b|11111111 11111111 11111111 11111110|]
        writeSafeRegister 1 1
        writeStatus statusC False
        execute $ T4 T4_ADC 0 1
        readStatus statusC
    c @?= False

t4adc3b :: TestTree
t4adc3b = testCase "[t4adc3b] C flag, set to 1" $ do
    c <- runTest $ do
        writeSafeRegister 0 [b|11111111 11111111 11111111 11111110|]
        writeSafeRegister 1 1
        writeStatus statusC True
        execute $ T4 T4_ADC 0 1
        readStatus statusC
    c @?= True

t4adc3c :: TestTree
t4adc3c = testProperty "[t4adc3c] C flag, set to 0 for small" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest), c) -> src /= dest ==> runPure $ do
        let n1' = setBit (clearBit (getLarge n1) 31) 1 - 1
            n2' = clearBit (getLarge n2) 31
        writeSafeRegister src n1'
        writeSafeRegister dest n2'
        writeStatus statusC c
        execute $ T4 T4_ADC src dest
        not <$> readStatus statusC

t4adc4a :: TestTree
t4adc4a = testCase "[t4adc4a] N flag, set to 1" $ do
    n <- runTest $ do
        writeSafeRegister 0 [b|01111111 11111111 11111111 11111110|]
        writeSafeRegister 1 1
        writeStatus statusC True
        execute $ T4 T4_ADC 0 1
        readStatus statusN
    n @?= True

t4adc4b :: TestTree
t4adc4b = testCase "[t4adc4b] N flag, set to 0" $ do
    n <- runTest $ do
        writeSafeRegister 0 [b|01111111 11111111 11111111 11111110|]
        writeSafeRegister 1 1
        writeStatus statusC False
        execute $ T4 T4_ADC 0 1
        readStatus statusN
    n @?= False

t4adc5a :: TestTree
t4adc5a = testProperty "[t4adc5a] V flag, set to 0 for positive & negative" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest), c) -> src /= dest ==> runPure $ do
        let n1' = setBit (clearBit (getLarge n1) 31) 1
            n2' = clearBit (setBit (getLarge n2) 31) 1
        writeSafeRegister src n1'
        writeSafeRegister dest n2'
        writeStatus statusC c
        execute $ T4 T4_ADC src dest
        not <$> readStatus statusV

t4adc5b :: TestTree
t4adc5b = testCase "[t4adc5b] V flag, set to 1" $ do
    v <- runTest $ do
        writeSafeRegister 0 [b|10000000 00000000 00000000 00000000|]
        writeSafeRegister 1 [b|11111111 11111111 11111111 11111111|]
        writeStatus statusC False
        execute $ T4 T4_ADC 0 1
        readStatus statusV
    v @?= True

t4adc5c :: TestTree
t4adc5c = testCase "[t4adc5c] V flag, set to 0" $ do
    v <- runTest $ do
        writeSafeRegister 0 [b|10000000 00000000 00000000 00000000|]
        writeSafeRegister 1 [b|11111111 11111111 11111111 11111111|]
        writeStatus statusC True
        execute $ T4 T4_ADC 0 1
        readStatus statusV
    v @?= False

t4adc6a :: TestTree
t4adc6a = testCase "[t4adc6a] corner case 1" $ do
    r <- runTest $ do
        writeSafeRegister 0 [b|10000000 00000000 00000000 00000000|]
        writeSafeRegister 1 [b|11111111 11111111 11111111 11111111|]
        writeStatus statusC False
        execute $ T4 T4_ADC 0 1
        readSafeRegister 1
    r @?= [b|01111111 11111111 11111111 11111111|]

t4adc6b :: TestTree
t4adc6b = testCase "[t4adc6b] corner case 2" $ do
    r <- runTest $ do
        writeSafeRegister 0 [b|10000000 00000000 00000000 00000000|]
        writeSafeRegister 1 [b|11111111 11111111 11111111 11111111|]
        writeStatus statusC True
        execute $ T4 T4_ADC 0 1
        readSafeRegister 1
    r @?= [b|10000000 00000000 00000000 00000000|]

-- t4.7
-------

t4sbc1 :: TestTree
t4sbc1 = testProperty "[t4sbc1] correctly sets register" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest), c) -> src /= dest ==> runPure $ do
        writeSafeRegister src (getLarge n1)
        writeSafeRegister dest (getLarge n2)
        writeStatus statusC c
        execute $ T4 T4_SBC src dest
        result <- readSafeRegister dest
        return $ result == getLarge n1 - getLarge n2 - fromIntegral (fromEnum (not c))

t4sbc2a :: TestTree
t4sbc2a = testProperty "[t4sbc2a] Z flag, set to 1, carry-in" $
    \(n, ThumbRegister src, ThumbRegister dest) -> src /= dest ==> runPure $ do
        writeSafeRegister src (getLarge n)
        writeSafeRegister dest (getLarge n)
        writeStatus statusC True
        execute $ T4 T4_SBC src dest
        readStatus statusZ

t4sbc2b :: TestTree
t4sbc2b = testProperty "[t4sbc2b] Z flag, set to 1, no carry-in" $
    \(n, ThumbRegister src, ThumbRegister dest) -> src /= dest ==> runPure $ do
        writeSafeRegister src (getLarge n + 1)
        writeSafeRegister dest (getLarge n)
        writeStatus statusC False
        execute $ T4 T4_SBC src dest
        readStatus statusZ

t4sbc2c :: TestTree
t4sbc2c = testProperty "[t4sbc2c] Z flag, set to 0, carry-in" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
    src /= dest && n1 /= n2 ==> runPure $ do
        writeSafeRegister src (getLarge n1)
        writeSafeRegister dest (getLarge n2)
        writeStatus statusC True
        execute $ T4 T4_SBC src dest
        not <$> readStatus statusZ

t4sbc2d :: TestTree
t4sbc2d = testProperty "[t4sbc2d] Z flag, set to 0, no carry-in" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
    src /= dest && n1 /= n2 + 1 && n1 /= n2 - 1 ==> runPure $ do
        writeSafeRegister src (getLarge n1)
        writeSafeRegister dest (getLarge n2)
        writeStatus statusC False
        execute $ T4 T4_SBC src dest
        not <$> readStatus statusZ

t4sbc2e :: TestTree
t4sbc2e = testProperty "[t4sbc2e] Z flag, set to 0, equal, no carry-in" $
    \(n, ThumbRegister src, ThumbRegister dest) -> src /= dest ==> runPure $ do
        writeSafeRegister src (getLarge n)
        writeSafeRegister dest (getLarge n)
        writeStatus statusC False
        execute $ T4 T4_SBC src dest
        not <$> readStatus statusZ
