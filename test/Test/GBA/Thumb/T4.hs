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
        [ t4and1a
        , t4and1b

        , t4and2a
        , t4and2b
        , t4and2c

        , t4and3a
        ]
    , testGroup "[t4.2] eor"
        [ t4eor1a
        , t4eor1b

        , t4eor2a
        , t4eor2b

        , t4eor3a
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
        [ t4adc1a

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
        [ t4sbc1a

        , t4sbc2a
        , t4sbc2b
        , t4sbc2c
        , t4sbc2d
        , t4sbc2e

        , t4sbc3a
        , t4sbc3b
        , t4sbc3c
        , t4sbc3d

        , t4sbc4a

        , t4sbc5a
        , t4sbc5b
        ]
    , testGroup "[t4.8] ror"
        [
        ]
    , testGroup "[t4.9] tst"
        [
        ]
    , testGroup "[t4.a] neg"
        [ t4neg1a
        , t4neg1b
        , t4neg1c

        , t4neg2a
        , t4neg2b

        , t4neg3a
        , t4neg3b

        , t4neg4a
        , t4neg4b
        , t4neg4c
        , t4neg4d

        , t4neg5a
        , t4neg5b
        , t4neg5c
        ]
    , testGroup "[t4.b] cmp"
        [ t4cmp1a

        , t4cmp2a
        , t4cmp2b

        , t4cmp3a
        , t4cmp3b
        , t4cmp3c

        , t4cmp4a

        , t4cmp5a
        , t4cmp5b
        ]
    , testGroup "[t4.c] cmn"
        [ t4cmn1a

        , t4cmn2a
        , t4cmn2b

        , t4cmn3a
        ]
    ]

t4parser1 :: TestTree
t4parser1 = testCase "[t4parser1] and" $ parseT [b|010000 0000 010 100|]
    @?= T4 T4_AND [b|010|] [b|100|]

t4parser2 :: TestTree
t4parser2 = testCase "[t4parser2] eor" $ parseT [b|010000 0001 010 100|]
    @?= T4 T4_EOR [b|010|] [b|100|]

t4parser3 :: TestTree
t4parser3 = testCase "[t4parser3] lsl" $ parseT [b|010000 0010 010 100|]
    @?= T4 T4_LSL [b|010|] [b|100|]

t4parser4 :: TestTree
t4parser4 = testCase "[t4parser4] lsr" $ parseT [b|010000 0011 010 100|]
    @?= T4 T4_LSR [b|010|] [b|100|]

t4parser5 :: TestTree
t4parser5 = testCase "[t4parser5] asr" $ parseT [b|010000 0100 011 101|]
    @?= T4 T4_ASR [b|011|] [b|101|]

t4parser6 :: TestTree
t4parser6 = testCase "[t4parser6] adc" $ parseT [b|010000 0101 011 101|]
    @?= T4 T4_ADC [b|011|] [b|101|]

t4parser7 :: TestTree
t4parser7 = testCase "[t4parser7] sbc" $ parseT [b|010000 0110 011 101|]
    @?= T4 T4_SBC [b|011|] [b|101|]

t4parser8 :: TestTree
t4parser8 = testCase "[t4parser8] ror" $ parseT [b|010000 0111 011 101|]
    @?= T4 T4_ROR [b|011|] [b|101|]

t4parser9 :: TestTree
t4parser9 = testCase "[t4parser9] tst" $ parseT [b|010000 1000 011 101|]
    @?= T4 T4_TST [b|011|] [b|101|]

t4parser10 :: TestTree
t4parser10 = testCase "[t4parser10] neg" $ parseT [b|010000 1001 011 101|]
    @?= T4 T4_NEG [b|011|] [b|101|]

t4parser11 :: TestTree
t4parser11 = testCase "[t4parser11] sbc" $ parseT [b|010000 1010 011 101|]
    @?= T4 T4_CMP [b|011|] [b|101|]

t4parser12 :: TestTree
t4parser12 = testCase "[t4parser12] ror" $ parseT [b|010000 1011 011 101|]
    @?= T4 T4_CMN [b|011|] [b|101|]

t4parser13 :: TestTree
t4parser13 = testCase "[t4parser13] orr" $ parseT [b|010000 1100 011 101|]
    @?= T4 T4_ORR [b|011|] [b|101|]

t4parser14 :: TestTree
t4parser14 = testCase "[t4parser14] mul" $ parseT [b|010000 1101 011 101|]
    @?= T4 T4_MUL [b|011|] [b|101|]

t4parser15 :: TestTree
t4parser15 = testCase "[t4parser15] bic" $ parseT [b|010000 1110 011 101|]
    @?= T4 T4_BIC [b|011|] [b|101|]

t4parser16 :: TestTree
t4parser16 = testCase "[t4parser16] mvn" $ parseT [b|010000 1111 011 101|]
    @?= T4 T4_MVN [b|011|] [b|101|]

-- t4.1
-------
t4and1a :: TestTree
t4and1a = testProperty "[t4and1a] correctly sets register (src /= dest)" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_AND src dest
        result <- readSafeRegister dest
        return $ result == n1 .&. n2

t4and1b :: TestTree
t4and1b = testProperty "[t4and1b] correctly sets register (src == dest)" $
    \(n, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r n
        execute $ T4 T4_AND r r
        result <- readSafeRegister r
        return $ result == n

t4and2a :: TestTree
t4and2a = testProperty "[t4and2a] Z flag" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_AND src dest
        result <- readSafeRegister dest
        z <- readStatus statusZ
        return $ z == (result == 0)

t4and2b :: TestTree
t4and2b = testProperty "[t4and2b] Z flag set with zero src" $
    \(n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        let n1 = 0
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_AND src dest
        result <- readSafeRegister dest
        readStatus statusZ

t4and2c :: TestTree
t4and2c = testProperty "[t4and2c] Z flag set with zero dest" $
    \(n1, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        let n2 = 0
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_AND src dest
        result <- readSafeRegister dest
        readStatus statusZ

t4and3a :: TestTree
t4and3a = testProperty "[t3and3a] N flag" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_AND src dest
        n <- readStatus statusN
        return $ n == testBit (getLarge n1 .&. getLarge n2) 31

t4eor1a :: TestTree
t4eor1a = testProperty "[t4eor1a] correctly sets register (src /= dest)" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_EOR src dest
        result <- readSafeRegister dest
        return $ result == getLarge n1 `xor` getLarge n2

t4eor1b :: TestTree
t4eor1b = testProperty "[t4eor1b] correctly sets register (src == dest)" $
    \(n, (ThumbRegister src)) -> runPure $ do
        writeSafeRegister src $ getLarge n
        writeSafeRegister src $ getLarge n
        execute $ T4 T4_EOR src src
        result <- readSafeRegister src
        return $ result == 0

t4eor2a :: TestTree
t4eor2a = testProperty "[t4eor2a] Z flag, set to 1" $
    \(n, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n
        writeSafeRegister dest n
        execute $ T4 T4_EOR src dest
        readStatus statusZ

t4eor2b :: TestTree
t4eor2b = testProperty "[t4eor2b] Z flag, set to 0" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) ->
      src /= dest && n1 /= n2 ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_EOR src dest
        not <$> readStatus statusZ

t4eor3a :: TestTree
t4eor3a = testProperty "[t4eor3a] N flag" $
    \(n1, n2, ThumbRegister src, ThumbRegister dest) -> src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_EOR src dest
        n <- readStatus statusN
        return $ n == testBit (getLarge n1 `xor` getLarge n2) 31

-- t4.6
t4adc1a :: TestTree
t4adc1a = testProperty "[t4adc1a] correctly sets register (src /= dest)" $
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

t4sbc1a :: TestTree
t4sbc1a = testProperty "[t4sbc1a] correctly sets register" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest), c) -> src /= dest ==> runPure $ do
        writeSafeRegister src (getLarge n1)
        writeSafeRegister dest (getLarge n2)
        writeStatus statusC c
        execute $ T4 T4_SBC src dest
        result <- readSafeRegister dest
        return $ result == getLarge n2 - getLarge n1 - fromIntegral (fromEnum (not c))

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
        writeSafeRegister src (getLarge n)
        writeSafeRegister dest (getLarge n + 1)
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

t4sbc3a :: TestTree
t4sbc3a = testCase "[t4sbc3a] C flag, set to 0" $ do
    c <- runTest $ do
        writeSafeRegister 0 1
        writeSafeRegister 1 1
        writeStatus statusC False
        execute $ T4 T4_SBC 0 1
        readStatus statusC
    c @?= False

t4sbc3b :: TestTree
t4sbc3b = testCase "[t4sbc3b] C flag, set to 1" $ do
    c <- runTest $ do
        writeSafeRegister 0 1
        writeSafeRegister 1 1
        writeStatus statusC True
        execute $ T4 T4_SBC 0 1
        readStatus statusC
    c @?= True

t4sbc3c :: TestTree
t4sbc3c = testCase "[t4sbc3c] C flag, set to 0, with dest = 0" $ do
    c <- runTest $ do
        writeSafeRegister 0 0
        writeSafeRegister 1 0
        writeStatus statusC False
        execute $ T4 T4_SBC 0 1
        readStatus statusC
    c @?= False

t4sbc3d :: TestTree
t4sbc3d = testCase "[t4sbc3d] C flag, set to 1, with dest = 0" $ do
    c <- runTest $ do
        writeSafeRegister 0 0
        writeSafeRegister 1 0
        writeStatus statusC True
        execute $ T4 T4_SBC 0 1
        readStatus statusC
    c @?= True

t4sbc4a :: TestTree
t4sbc4a = testProperty "[t4sbc4a] N flag" $
    \(n1, n2, c, ThumbRegister src, ThumbRegister dest) -> src /= dest ==> runPure $ do
        writeSafeRegister src (getLarge n1)
        writeSafeRegister dest (getLarge n2)
        writeStatus statusC c
        execute $ T4 T4_SBC src dest
        res <- readSafeRegister dest
        n <- readStatus statusN
        return $ n == testBit res 31

t4sbc5a :: TestTree
t4sbc5a = testCase "[t4sbc5a] V flag, set to 0" $ do
    v <- runTest $ do
        writeSafeRegister 0 1
        writeSafeRegister 1 0x80000001
        writeStatus statusC True
        execute $ T4 T4_SBC 0 1
        readStatus statusV
    v @?= False

t4sbc5b :: TestTree
t4sbc5b = testCase "[t4sbc5b] V flag, set to 1" $ do
    v <- runTest $ do
        writeSafeRegister 0 1
        writeSafeRegister 1 0x80000001
        writeStatus statusC False
        execute $ T4 T4_SBC 0 1
        readStatus statusV
    v @?= True

t4neg1a :: TestTree
t4neg1a = testProperty "[t4neg1a] correctly sets register" $
    \(n1, n2, ThumbRegister src, ThumbRegister dest) -> src /= dest ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_NEG src dest
        result <- readSafeRegister dest
        return $ result == 0 - n1

t4neg1b :: TestTree
t4neg1b = testCase "[t4neg1b] correctly sets register, src == 0" $ do
    res <- runTest $ do
        writeSafeRegister 0 0
        execute $ T4 T4_NEG 0 1
        readSafeRegister 1
    res @?= 0

t4neg1c :: TestTree
t4neg1c = testCase "[t4neg1c] correctly sets register, src == 0x80000000" $ do
    res <- runTest $ do
        writeSafeRegister 0 0x80000000
        execute $ T4 T4_NEG 0 1
        readSafeRegister 1
    res @?= 0x80000000

t4neg2a :: TestTree
t4neg2a = testProperty "[t4neg2a] Z flag, src /= 0" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
    src /= dest && n1 /= 0 ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_NEG src dest
        not <$> readStatus statusZ

t4neg2b :: TestTree
t4neg2b = testCase "[t4neg2b] Z flag, src == 0" $ do
    z <- runTest $ do
        writeSafeRegister 0 0
        execute $ T4 T4_NEG 0 1
        readStatus statusZ
    z @?= True

t4neg3a :: TestTree
t4neg3a = testProperty "[t4neg3a] C flag, src /= 0" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
    src /= dest && n1 /= 0 ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_NEG src dest
        not <$> readStatus statusC

t4neg3b :: TestTree
t4neg3b = testCase "[t4neg3b] C flag, src == 0" $ do
    c <- runTest $ do
        writeSafeRegister 0 0
        execute $ T4 T4_NEG 0 1
        readStatus statusC
    c @?= True

t4neg4a :: TestTree
t4neg4a = testProperty "[t4neg4a] N flag" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
    src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_NEG src dest
        n <- readStatus statusN
        return $ n == testBit (0 - getLarge n1) 31

t4neg4b :: TestTree
t4neg4b = testProperty "[t4neg4b] N flag, src /= 0, src /= 0x80000000" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
    src /= dest && n1 /= 0 && n1 /= 0x80000000 ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_NEG src dest
        n <- readStatus statusN
        return $ n == not (testBit (getLarge n1) 31)

t4neg4c :: TestTree
t4neg4c = testCase "[t4neg4c] N flag, src == 0" $ do
    n <- runTest $ do
        writeSafeRegister 0 0
        execute $ T4 T4_NEG 0 1
        readStatus statusN
    n @?= False

t4neg4d :: TestTree
t4neg4d = testCase "[t4neg4d] N flag, src == 0x80000000" $ do
    n <- runTest $ do
        writeSafeRegister 0 0x80000000
        execute $ T4 T4_NEG 0 1
        readStatus statusN
    n @?= True

t4neg5a :: TestTree
t4neg5a = testProperty "[t4neg5a] V flag, src /= 0x80000000" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
     src /= dest && n1 /= 0x80000000 ==> runPure $ do
         writeSafeRegister src $ getLarge n1
         writeSafeRegister dest $ getLarge n2
         execute $ T4 T4_NEG src dest
         not <$> readStatus statusV

t4neg5b :: TestTree
t4neg5b = testCase "[t4neg5b] V flag, src == 0x80000000" $ do
    v <- runTest $ do
        writeSafeRegister 0 0x80000000
        execute $ T4 T4_NEG 0 1
        readStatus statusV
    v @?= True

t4neg5c :: TestTree
t4neg5c = testCase "[t4neg5c] V flag, src == 0x80000001" $ do
    v <- runTest $ do
        writeSafeRegister 0 0x80000001
        execute $ T4 T4_NEG 0 1
        readStatus statusV
    v @?= False

t4cmp1a :: TestTree
t4cmp1a = testProperty "[t4cmp1a] does not change register" $
    \(n1, n2, ThumbRegister src, ThumbRegister dest) -> src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_CMP src dest
        result <- readSafeRegister dest
        return $ result == getLarge n2

t4cmp2a :: TestTree
t4cmp2a = testProperty "[t4cmp2a] Z flag, src val /= dest val" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
    src /= dest && n1 /= n2 ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_CMP src dest
        not <$> readStatus statusZ

t4cmp2b :: TestTree
t4cmp2b = testProperty "[t4cmp2a] Z flag, src val == dest val" $
  \(n1, ThumbRegister src, ThumbRegister dest) ->
    src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n1
        execute $ T4 T4_CMP src dest
        readStatus statusZ

t4cmp3a :: TestTree
t4cmp3a = testProperty "[t4cmp3a] C flag, dest val > src val" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
    src /= dest && n2 > n1 ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_CMP src dest
        readStatus statusC

t4cmp3b :: TestTree
t4cmp3b = testProperty "[t4cmp3b] C flag, dest val == src val" $
  \(n1, ThumbRegister src, ThumbRegister dest) ->
    src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n1
        execute $ T4 T4_CMP src dest
        readStatus statusC

t4cmp3c :: TestTree
t4cmp3c = testProperty "[t4cmp3c] C flag, dest val < src val" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
    src /= dest && n2 < n1 ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_CMP src dest
        not <$> readStatus statusC

t4cmp4a :: TestTree
t4cmp4a = testProperty "[t4cmp4a] N flag" $
    \(n1, n2, ThumbRegister src, ThumbRegister dest) -> src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_CMP src dest
        n <- readStatus statusN
        return $ n == testBit (getLarge n2 - getLarge n1) 31

t4cmp5a :: TestTree
t4cmp5a = testCase "[t4cmp5a] V flag, set to 0" $ do
    v <- runTest $ do
        writeSafeRegister 0 0xFFFFFFFF
        writeSafeRegister 1 0x7FFFFFFE
        execute $ T4 T4_CMP 0 1
        readStatus statusV
    v @?= False

t4cmp5b :: TestTree
t4cmp5b = testCase "[t4cmp5b] V flag, set to 1" $ do
    v <- runTest $ do
        writeSafeRegister 0 0xFFFFFFFE
        writeSafeRegister 1 0x7FFFFFFE
        execute $ T4 T4_CMP 0 1
        readStatus statusV
    v @?= True

t4cmn1a :: TestTree
t4cmn1a = testProperty "[t4cmn1a] does not change register" $
    \(n1, n2, ThumbRegister src, ThumbRegister dest) -> src /= dest ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ T4 T4_CMN src dest
        result <- readSafeRegister dest
        return $ result == n2

t4cmn2a :: TestTree
t4cmn2a = testProperty "[t4cmn2a] Z flag, set to 0" $
  \(n1, n2, ThumbRegister src, ThumbRegister dest) ->
    src /= dest && n2 /= (-n1) ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_CMN src dest
        not <$> readStatus statusZ

t4cmn2b :: TestTree
t4cmn2b = testProperty "[t4cmn2b] Z flag, set to 1" $
  \(n1, ThumbRegister src, ThumbRegister dest) ->
    src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ -(getLarge n1)
        execute $ T4 T4_CMN src dest
        readStatus statusZ

t4cmn3a :: TestTree
t4cmn3a = testProperty "[t4cmn3a] C,N,V flag" $
    \(n1, n2, ThumbRegister src, ThumbRegister dest) -> src /= dest ==> runPure $ do
        writeSafeRegister src $ getLarge n1
        writeSafeRegister dest $ getLarge n2
        execute $ T4 T4_CMN src dest
        cnv <- (,,) <$> readStatus statusC <*> readStatus statusN <*> readStatus statusV
        writeSafeRegister src $ -(getLarge n1)
        execute $ T4 T4_CMP src dest
        cnv' <- (,,) <$> readStatus statusC <*> readStatus statusN <*> readStatus statusV
        return $ cnv == cnv'
