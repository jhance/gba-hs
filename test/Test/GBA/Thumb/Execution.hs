{-# LANGUAGE ScopedTypeVariables #-}
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
import           Game.GBA.Monad
import           Game.GBA.Register
import           Game.GBA.Thumb.Instruction
import           Game.GBA.Thumb.Execution

import           Test.HUnit hiding (assert)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck hiding ((.&.))
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Modifiers

-- | A register from 0 to 7.
newtype ThumbRegister = ThumbRegister RegisterID
    deriving (Read, Show, Eq, Ord)

instance Arbitrary ThumbRegister where
    arbitrary = ThumbRegister . (`rem` 8) . getNonNegative <$> arbitrary
    shrink = const []

runTest :: GBA RealWorld a -> IO a
runTest gba = stToIO $ do
    context <- makeGBAContext
    runGBA context (bootForTest Thumb >> gba)

runPure :: forall a. (forall s. GBA s a) -> a
runPure gba = runST $ do
    context <- makeGBAContext
    runGBA context (bootForTest Thumb >> gba)

tests :: TestTree
tests = testGroup "execution"
    [ testGroup "shift register (t1)"
      [ testGroup "lsl (t1.1)" 
          [ shiftRegLSL1
          , shiftRegLSL2
          , shiftRegLSL3
          , shiftRegLSL4
          , shiftRegLSL5
          , shiftRegLSL6
          , shiftRegLSL7
          , shiftRegLSL8
          ]
      , testGroup "lsr (t1.2)"
          [ shiftRegLSR1
          , shiftRegLSR2
          , shiftRegLSR3
          , shiftRegLSR4
          , shiftRegLSR5
          ]
      , testGroup "asr (t1.3)"
          [ shiftRegASR1
          , shiftRegASR2
          , shiftRegASR3
          , shiftRegASR4
          ]
      ]
    , testGroup "add, sub (t2)"
      [ testGroup "add (t2.1)"
          [ tasAdd1
          , tasAdd2
          , tasAdd3
          , tasAdd4
          ]
      , testGroup "sub (t2.2)"
          [ tasSub1
          --, tasSub2
          ]
      ]
    , testGroup "mov, cmp, add, subtract, 8-bit literal (t3)"
      [ testGroup "mov (t3.1)"
          [ tmcasMov1
          , tmcasMov2
          , tmcasMov3
          , tmcasMov4
          ]
      , testGroup "cmp (t3.2)"
          [ tmcasCmp1
          , tmcasCmp2
          , tmcasCmp3
          , tmcasCmp4
          , tmcasCmp5
          , tmcasCmp6
          , tmcasCmp7
          , tmcasCmp8
          , tmcasCmp9
          , tmcasCmp10
          , tmcasCmp11
          , tmcasCmp12
          , tmcasCmp13
          ]
      , testGroup "add (t3.3)"
          [ tmcasAdd1
          , tmcasAdd2
          , tmcasAdd3
          , tmcasAdd4
          , tmcasAdd5
          , tmcasAdd6
          , tmcasAdd7
          , tmcasAdd8
          , tmcasAdd9
          , tmcasAdd10
          , tmcasAdd11
          , tmcasAdd12
          , tmcasAdd13
          ]
      -- sub is basically cmp but with modified out register
      -- so testing it extensively is fairly pointless
      , testGroup "sub (t3.4)"
          [ tmcasSub1
          ]
      ]
    , testGroup "alu (t4)"
      [ testGroup "and (t4.1)"
          [ taluAnd1
          , taluAnd2
          ]
      ]
    ]

-- t1
-----
getShiftResult :: RegisterID -> GBA s (Word32, Bool, Bool, Bool)
getShiftResult reg = (,,,)
    <$> readRegister reg
    <*> readStatus statusC
    <*> readStatus statusZ
    <*> readStatus statusN

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
    <*> readStatus statusC
    <*> readStatus statusZ
    <*> readStatus statusN
    <*> readStatus statusV

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

-- t2.2
-------
tasSub1 :: TestTree
tasSub1 = testCase "tas sub simple register" $ do
    (dest, c, z, n, v) <- runTest $ do
        writeRegister [b|000|] 5
        writeRegister [b|010|] 3
        execute (TAS TASO_REG TASO_SUB [b|010|] [b|000|] [b|001|])
        getTASResult [b|001|]
    dest @?= 2
    c @?= True -- Is this right?!
    z @?= False
    n @?= False
    v @?= False

-- t3
-----

-- t3.1
-------
tmcasMov1 :: TestTree
tmcasMov1 = testProperty "tmcas mov always clears status-N" . monadicIO $ do
    num <- pick arbitrary
    n <- run . runTest $ do
        writeSafeRegister [b|000|] 5
        execute $ TMCAS TMCASO_MOV [b|000|] num
        readStatus statusN
    assert $ not n

tmcasMov2 :: TestTree
tmcasMov2 = testProperty "tmcas mov sets register to value" . monadicIO $ do
    num <- pick arbitrary
    n <- run . runTest $ do
        writeSafeRegister [b|000|] 5
        execute $ TMCAS TMCASO_MOV [b|000|] num
        readSafeRegister [b|000|]
    assert $ n == num

tmcasMov3 :: TestTree
tmcasMov3 = testCase "tmcas mov zero" $ do
    z <- runTest $ do
        writeSafeRegister [b|000|] 5
        execute $ TMCAS TMCASO_MOV [b|000|] 0
        readStatus statusZ
    z @?= True

tmcasMov4 :: TestTree
tmcasMov4 = testCase "tmcas mov nonzero" $ do
    z <- runTest $ do
        writeSafeRegister [b|000|] 5
        execute $ TMCAS TMCASO_MOV [b|000|] 10
        readStatus statusZ
    z @?= False

-- t3.2
-------
tmcasCmp1 :: TestTree
tmcasCmp1 = testProperty "tmcas cmp does not change register" $
  \(n, (ThumbRegister r), k :: Word8) -> runPure $ do
      writeSafeRegister r n
      execute $ TMCAS TMCASO_CMP r (fromIntegral k)
      n' <- readSafeRegister r
      return $ n == n'
            
tmcasCmp2 :: TestTree
tmcasCmp2 = testProperty "tmcas cmp Z flag equal" $
    \(n :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r $ fromIntegral n
        execute $ TMCAS TMCASO_CMP r (fromIntegral n)
        readStatus statusZ

tmcasCmp3 :: TestTree
tmcasCmp3 = testProperty "tmcas cmp Z flag nonequal" $
    \(n :: Word8, k :: Word8, (ThumbRegister r)) -> n /= k ==> runPure $ do
        writeSafeRegister r $ fromIntegral n
        execute $ TMCAS TMCASO_CMP r (fromIntegral k)
        not <$> readStatus statusZ

tmcasCmp4 :: TestTree
tmcasCmp4 = testProperty "tmcas cmp C flag equal" $
    \(n :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r $ fromIntegral n
        execute $ TMCAS TMCASO_CMP r (fromIntegral n)
        readStatus statusC

tmcasCmp5 :: TestTree
tmcasCmp5 = testProperty "tmcas cmp C flag gte" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> n > fromIntegral k ==> runPure $ do
        writeSafeRegister r n
        execute $ TMCAS TMCASO_CMP r (fromIntegral k)
        readStatus statusC

tmcasCmp6 :: TestTree
tmcasCmp6 = testProperty "tmcas cmp C flag lte" $
    \(n :: Word8, k :: Word8, (ThumbRegister r)) -> n < k ==> runPure $ do
        writeSafeRegister r (fromIntegral n)
        execute $ TMCAS TMCASO_CMP r (fromIntegral k)
        not <$> readStatus statusC

tmcasCmp7 :: TestTree
tmcasCmp7 = testCase "tmcas cmp V flag on overflow" $ do
    v <- runTest $ do
        writeSafeRegister 0 [b|10000000 00000000 00000000 00000111|]
        execute $ TMCAS TMCASO_CMP 0 [b|1000|]
        readStatus statusV
    v @?= True

tmcasCmp8 :: TestTree
tmcasCmp8 = testCase "tmcas cmp V flag on no overflow" $ do
    v <- runTest $ do
        writeSafeRegister 1 [b|11111111 11111111 11111111 11111100|]
        execute $ TMCAS TMCASO_CMP 1 [b|11|]
        readStatus statusV
    v @?= False

tmcasCmp9 :: TestTree
tmcasCmp9 = testProperty "tmcas cmp N flag with two positive, set to 1" $
    \(n :: Word8, k :: Word8, (ThumbRegister r)) -> n < k ==> runPure $ do
        writeSafeRegister r (fromIntegral n)
        execute $ TMCAS TMCASO_CMP r (fromIntegral k)
        readStatus statusN

tmcasCmp10 :: TestTree
tmcasCmp10 = testProperty "tmcas cmp N flag with two positive, set to 0" $
    \(n :: Word8, k :: Word8, (ThumbRegister r)) -> n > k ==> runPure $ do
        writeSafeRegister r (fromIntegral n)
        execute $ TMCAS TMCASO_CMP r (fromIntegral k)
        not <$> readStatus statusN

tmcasCmp11 :: TestTree
tmcasCmp11 = testProperty "tmcas cmp N flag with two equal, set to 0" $
    \(n :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r (fromIntegral n)
        execute $ TMCAS TMCASO_CMP r (fromIntegral n)
        not <$> readStatus statusN

tmcasCmp12 :: TestTree
tmcasCmp12 = testCase "tmcas cmp N flag with negative, set to 1" $ do
    n <- runTest $ do
        writeSafeRegister 0 [b|10000000 00000000 00000000 00000111|]
        execute $ TMCAS TMCASO_CMP 0 [b|11|]
        readStatus statusN
    n @?= True

tmcasCmp13 :: TestTree
tmcasCmp13 = testCase "tmcas cmp N flag with negative, set to 0" $ do
    n <- runTest $ do
        writeSafeRegister 1 [b|10000000 00000000 00000000 00000111|]
        execute $ TMCAS TMCASO_CMP 1 [b|1111|]
        readStatus statusN
    n @?= False

-- t3.3
-------

tmcasAdd1 :: TestTree
tmcasAdd1 = testProperty "tmcas add correctly sets register" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r n
        execute $ TMCAS TMCASO_ADD r (fromIntegral k)
        result <- readSafeRegister r
        return $ result == n + fromIntegral k

tmcasAdd2 :: TestTree
tmcasAdd2 = testProperty "tmcas add Z flag, set to 1" $
    \(k :: Word8, (ThumbRegister r)) -> runPure $ do
        let k' = fromIntegral k :: Word32
        writeSafeRegister r (maxBound - k' + 1)
        execute $ TMCAS TMCASO_ADD r k'
        readStatus statusZ

tmcasAdd3 :: TestTree
tmcasAdd3 = testProperty "tmcas add Z flag, set to 0" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> n + fromIntegral k /= 0 ==> runPure $ do
        writeSafeRegister r n
        execute $ TMCAS TMCASO_ADD r (fromIntegral k)
        not <$> readStatus statusZ

tmcasAdd4 :: TestTree
tmcasAdd4 = testCase "tmcas add Z flag, 0 + 0 = 0" $ do
    z <- runTest $ do
        writeSafeRegister 0 0
        execute $ TMCAS TMCASO_ADD 0 0
        readStatus statusZ
    z @?= True

tmcasAdd5 :: TestTree
tmcasAdd5 = testProperty "tmcas add C flag, for small values" $
    \(n :: Word16, k :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r (fromIntegral n)
        execute $ TMCAS TMCASO_ADD r (fromIntegral k)
        not <$> readStatus statusC

tmcasAdd6 :: TestTree
tmcasAdd6 = testProperty "tmcas add C flag, for large values" $
    \(n :: Word8, k :: Word8, (ThumbRegister r)) -> n < k ==> runPure $ do
        let n' = maxBound - fromIntegral n :: Word32
        writeSafeRegister r n'
        execute $ TMCAS TMCASO_ADD r (fromIntegral k)
        readStatus statusC

tmcasAdd7 :: TestTree
tmcasAdd7 = testProperty "tmcas add C flag, when zero" $
    \(k :: Word8, (ThumbRegister r)) -> k /= 0 ==> runPure $ do
        let k' = fromIntegral k :: Word32
        writeSafeRegister r (maxBound - k' + 1)
        execute $ TMCAS TMCASO_ADD r k'
        readStatus statusC

tmcasAdd8 :: TestTree
tmcasAdd8 = testProperty "tmcas add V flag is 0 when register is negative" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r $ setBit n 31
        execute $ TMCAS TMCASO_ADD r (fromIntegral k)
        not <$> readStatus statusV

tmcasAdd9 :: TestTree
tmcasAdd9 = testProperty "tmcas add V flag is 0 when literal is 0" $
    \(n :: Word32, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r n
        execute $ TMCAS TMCASO_ADD r 0
        not <$> readStatus statusV

tmcasAdd10 :: TestTree
tmcasAdd10 = testCase "tmcas add V flag, set to 0" $ do
    v <- runTest $ do
        writeSafeRegister [b|000|] [b|01111111 11111111 11111111 11111110|]
        execute $ TMCAS TMCASO_ADD [b|000|] [b|1|]
        readStatus statusV
    v @?= False

tmcasAdd11 :: TestTree
tmcasAdd11 = testCase "tmcas add V flag, set to 1" $ do
    v <- runTest $ do
        writeSafeRegister [b|000|] [b|01111111 11111111 11111111 11111110|]
        execute $ TMCAS TMCASO_ADD [b|000|] [b|10|]
        readStatus statusV
    v @?= True

tmcasAdd12 :: TestTree
tmcasAdd12 = testProperty "tmcas add N flag" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> runPure $ do
        let k' = fromIntegral k :: Word32
        writeSafeRegister r n
        execute $ TMCAS TMCASO_ADD r k'
        result <- readSafeRegister r
        sn <- readStatus statusN
        return $ sn == testBit (n + k') 31

tmcasAdd13 :: TestTree
tmcasAdd13 = testProperty "tmcas add N flag (large)" $
    \(n, k :: Word8, (ThumbRegister r)) -> runPure $ do
        let k' = fromIntegral k :: Word32
        writeSafeRegister r $ getLarge n
        execute $ TMCAS TMCASO_ADD r k'
        result <- readSafeRegister r
        sn <- readStatus statusN
        return $ sn == testBit (getLarge n + k') 31

-- t3.4
-------

tmcasSub1 :: TestTree
tmcasSub1 = testProperty "tmcas sub correctly sets register" $
    \(n :: Word32, k :: Word8, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r n
        execute $ TMCAS TMCASO_SUB r (fromIntegral k)
        result <- readSafeRegister r
        return $ result == n + complement (fromIntegral k) + 1 
                    && result == n - fromIntegral k

-- t4
-----

-- t4.1
-------

taluAnd1 :: TestTree
taluAnd1 = testProperty "talu and correctly sets register (src /= dest)" $
    \(n1, n2, (ThumbRegister src), (ThumbRegister dest)) -> src /= dest ==> runPure $ do
        writeSafeRegister src n1
        writeSafeRegister dest n2
        execute $ TALU TALU_AND src dest
        result <- readSafeRegister dest
        return $ result == n1 .&. n2

taluAnd2 :: TestTree
taluAnd2 = testProperty "talu and correctly sets register (src == dest)" $
    \(n, (ThumbRegister r)) -> runPure $ do
        writeSafeRegister r n
        execute $ TALU TALU_AND r r
        result <- readSafeRegister r
        return $ result == n
