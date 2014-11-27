{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.GBA.Thumb.T3
    (tests)
where

import Control.Applicative
import Data.Bits
import Data.Word
import Language.Literals.Binary

import Test.HUnit
import Test.QuickCheck
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
tests = testGroup "[t3] mov, cmp, add, sub"
    [ testGroup "[t3.0] parser"
        [ t3parser1
        , t3parser2
        , t3parser3
        , t3parser4
        ]
    , testGroup "[t3.1] mov"
        [ t3mov1
        , t3mov2
        , t3mov3
        , t3mov4
        ]
    , testGroup "[t3.2] cmp"
        [ t3cmp1
        , t3cmp2
        , t3cmp3
        , t3cmp4
        , t3cmp5
        , t3cmp6
        , t3cmp7
        , t3cmp8
        , t3cmp9
        , t3cmp10
        , t3cmp11
        , t3cmp12
        , t3cmp13
        ]
    , testGroup "[t3.3] add"
        [ t3add1
        , t3add2
        , t3add3
        , t3add4
        , t3add5
        , t3add6
        , t3add7
        , t3add8
        , t3add9
        , t3add10
        , t3add11
        , t3add12
        , t3add13
        ]
    , testGroup "[t3.4] sub"
        [ t3sub1
        ]
    ]

t3parser1 :: TestTree
t3parser1 = testCase "move immediate" $ parseT [b|001 00 010 00000011|]
    @?= Just (T3 T3_MOV [b|010|] [b|00000011|])

t3parser2 :: TestTree
t3parser2 = testCase "cmp immediate" $ parseT [b|001 01 011 11111111|]
    @?= Just (T3 T3_CMP [b|011|] [b|11111111|])

t3parser3 :: TestTree
t3parser3 = testCase "add immediate" $ parseT [b|001 10 000 10101010|]
    @?= Just (T3 T3_ADD [b|000|] [b|10101010|])

t3parser4 :: TestTree
t3parser4 = testCase "subtract immediate" $ parseT [b|001 11 111 10000101|]
    @?= Just (T3 T3_SUB [b|111|] [b|10000101|])

-- t3.1
-------
t3mov1 :: TestTree
t3mov1 = testProperty "mov always clears status-N" $
    \(num, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r 5
        execute $ T3 T3_MOV r num
        not <$> readStatus statusN

t3mov2 :: TestTree
t3mov2 = testProperty "mov sets register to value" $
    \(num, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r 5
        execute $ T3 T3_MOV r num
        res <- readSafeRegister r
        return $ res == num

t3mov3 :: TestTree
t3mov3 = testCase "mov zero" $ do
    z <- runTest $ do
        writeSafeRegister [b|000|] 5
        execute $ T3 T3_MOV [b|000|] 0
        readStatus statusZ
    z @?= True

t3mov4 :: TestTree
t3mov4 = testCase "mov nonzero" $ do
    z <- runTest $ do
        writeSafeRegister [b|000|] 5
        execute $ T3 T3_MOV [b|000|] 10
        readStatus statusZ
    z @?= False

-- t3.2
-------
t3cmp1 :: TestTree
t3cmp1 = testProperty "cmp does not change register" $
  \(n, (ThumbRegister r), k :: Word8) -> runPure $ do
      writeSafeRegister r n
      execute $ T3 T3_CMP r (fromIntegral k)
      n' <- readSafeRegister r
      return $ n == n'
            
t3cmp2 :: TestTree
t3cmp2 = testProperty "cmp Z flag equal" $
    \(n :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r $ fromIntegral n
        execute $ T3 T3_CMP r (fromIntegral n)
        readStatus statusZ

t3cmp3 :: TestTree
t3cmp3 = testProperty "cmp Z flag nonequal" $
    \(n :: Word8, k :: Word8, (ThumbRegister r)) -> n /= k ==> runPure $ do
        writeSafeRegister r $ fromIntegral n
        execute $ T3 T3_CMP r (fromIntegral k)
        not <$> readStatus statusZ

t3cmp4 :: TestTree
t3cmp4 = testProperty "cmp C flag equal" $
    \(n :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r $ fromIntegral n
        execute $ T3 T3_CMP r (fromIntegral n)
        readStatus statusC

t3cmp5 :: TestTree
t3cmp5 = testProperty "cmp C flag gte" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> n > fromIntegral k ==> runPure $ do
        writeSafeRegister r n
        execute $ T3 T3_CMP r (fromIntegral k)
        readStatus statusC

t3cmp6 :: TestTree
t3cmp6 = testProperty "cmp C flag lte" $
    \(n :: Word8, k :: Word8, (ThumbRegister r)) -> n < k ==> runPure $ do
        writeSafeRegister r (fromIntegral n)
        execute $ T3 T3_CMP r (fromIntegral k)
        not <$> readStatus statusC

t3cmp7 :: TestTree
t3cmp7 = testCase "cmp V flag on overflow" $ do
    v <- runTest $ do
        writeSafeRegister 0 [b|10000000 00000000 00000000 00000111|]
        execute $ T3 T3_CMP 0 [b|1000|]
        readStatus statusV
    v @?= True

t3cmp8 :: TestTree
t3cmp8 = testCase "cmp V flag on no overflow" $ do
    v <- runTest $ do
        writeSafeRegister 1 [b|11111111 11111111 11111111 11111100|]
        execute $ T3 T3_CMP 1 [b|11|]
        readStatus statusV
    v @?= False

t3cmp9 :: TestTree
t3cmp9 = testProperty "cmp N flag with two positive, set to 1" $
    \(n :: Word8, k :: Word8, (ThumbRegister r)) -> n < k ==> runPure $ do
        writeSafeRegister r (fromIntegral n)
        execute $ T3 T3_CMP r (fromIntegral k)
        readStatus statusN

t3cmp10 :: TestTree
t3cmp10 = testProperty "cmp N flag with two positive, set to 0" $
    \(n :: Word8, k :: Word8, (ThumbRegister r)) -> n > k ==> runPure $ do
        writeSafeRegister r (fromIntegral n)
        execute $ T3 T3_CMP r (fromIntegral k)
        not <$> readStatus statusN

t3cmp11 :: TestTree
t3cmp11 = testProperty "cmp N flag with two equal, set to 0" $
    \(n :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r (fromIntegral n)
        execute $ T3 T3_CMP r (fromIntegral n)
        not <$> readStatus statusN

t3cmp12 :: TestTree
t3cmp12 = testCase "cmp N flag with negative, set to 1" $ do
    n <- runTest $ do
        writeSafeRegister 0 [b|10000000 00000000 00000000 00000111|]
        execute $ T3 T3_CMP 0 [b|11|]
        readStatus statusN
    n @?= True

t3cmp13 :: TestTree
t3cmp13 = testCase "cmp N flag with negative, set to 0" $ do
    n <- runTest $ do
        writeSafeRegister 1 [b|10000000 00000000 00000000 00000111|]
        execute $ T3 T3_CMP 1 [b|1111|]
        readStatus statusN
    n @?= False

-- t3.3
-------

t3add1 :: TestTree
t3add1 = testProperty "add correctly sets register" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r n
        execute $ T3 T3_ADD r (fromIntegral k)
        result <- readSafeRegister r
        return $ result == n + fromIntegral k

t3add2 :: TestTree
t3add2 = testProperty "add Z flag, set to 1" $
    \(k :: Word8, (ThumbRegister r)) -> runPure $ do
        let k' = fromIntegral k :: Word32
        writeSafeRegister r (maxBound - k' + 1)
        execute $ T3 T3_ADD r k'
        readStatus statusZ

t3add3 :: TestTree
t3add3 = testProperty "add Z flag, set to 0" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> n + fromIntegral k /= 0 ==> runPure $ do
        writeSafeRegister r n
        execute $ T3 T3_ADD r (fromIntegral k)
        not <$> readStatus statusZ

t3add4 :: TestTree
t3add4 = testCase "add Z flag, 0 + 0 = 0" $ do
    z <- runTest $ do
        writeSafeRegister 0 0
        execute $ T3 T3_ADD 0 0
        readStatus statusZ
    z @?= True

t3add5 :: TestTree
t3add5 = testProperty "add C flag, for small values" $
    \(n :: Word16, k :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r (fromIntegral n)
        execute $ T3 T3_ADD r (fromIntegral k)
        not <$> readStatus statusC

t3add6 :: TestTree
t3add6 = testProperty "add C flag, for large values" $
    \(n :: Word8, k :: Word8, (ThumbRegister r)) -> n < k ==> runPure $ do
        let n' = maxBound - fromIntegral n :: Word32
        writeSafeRegister r n'
        execute $ T3 T3_ADD r (fromIntegral k)
        readStatus statusC

t3add7 :: TestTree
t3add7 = testProperty "add C flag, when zero" $
    \(k :: Word8, (ThumbRegister r)) -> k /= 0 ==> runPure $ do
        let k' = fromIntegral k :: Word32
        writeSafeRegister r (maxBound - k' + 1)
        execute $ T3 T3_ADD r k'
        readStatus statusC

t3add8 :: TestTree
t3add8 = testProperty "add V flag is 0 when register is negative" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r $ setBit n 31
        execute $ T3 T3_ADD r (fromIntegral k)
        not <$> readStatus statusV

t3add9 :: TestTree
t3add9 = testProperty "add V flag is 0 when literal is 0" $
    \(n :: Word32, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r n
        execute $ T3 T3_ADD r 0
        not <$> readStatus statusV

t3add10 :: TestTree
t3add10 = testCase "add V flag, set to 0" $ do
    v <- runTest $ do
        writeSafeRegister [b|000|] [b|01111111 11111111 11111111 11111110|]
        execute $ T3 T3_ADD [b|000|] [b|1|]
        readStatus statusV
    v @?= False

t3add11 :: TestTree
t3add11 = testCase "add V flag, set to 1" $ do
    v <- runTest $ do
        writeSafeRegister [b|000|] [b|01111111 11111111 11111111 11111110|]
        execute $ T3 T3_ADD [b|000|] [b|10|]
        readStatus statusV
    v @?= True

t3add12 :: TestTree
t3add12 = testProperty "add N flag" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> runPure $ do
        let k' = fromIntegral k :: Word32
        writeSafeRegister r n
        execute $ T3 T3_ADD r k'
        result <- readSafeRegister r
        sn <- readStatus statusN
        return $ sn == testBit (n + k') 31

t3add13 :: TestTree
t3add13 = testProperty "add N flag (large)" $
    \(n, k :: Word8, (ThumbRegister r)) -> runPure $ do
        let k' = fromIntegral k :: Word32
        writeSafeRegister r $ getLarge n
        execute $ T3 T3_ADD r k'
        result <- readSafeRegister r
        sn <- readStatus statusN
        return $ sn == testBit (getLarge n + k') 31

-- t3.4
-------

t3sub1 :: TestTree
t3sub1 = testProperty "sub correctly sets register" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r n
        execute $ T3 T3_SUB r (fromIntegral k)
        result <- readSafeRegister r
        return $ result == n + complement (fromIntegral k) + 1 
                    && result == n - fromIntegral k