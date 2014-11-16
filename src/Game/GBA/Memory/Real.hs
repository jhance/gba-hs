-- | Access to @RealAddress@ from within the @GBA@ monad.
module Game.GBA.Memory.Real
    ( RealAddress
    , Segment
    , SegmentOffset
    , readReal8
    , readReal16
    , readReal32
    , writeReal8
    , writeReal16
    , writeReal32
    )
where

import           Debug.Trace

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Base
import           Data.Bits
import           Data.Word
import           Game.GBA.MemoryMap
import           Game.GBA.Monad

-- | Write to a real address. (in 8-bit mode)
writeReal8 :: RealAddress -> Word8 -> GBA s ()
writeReal8 addr val = do
    mem <- view $ gbaMemoryMap
    liftBase $ writeRaw mem addr val

-- | Write to a real address. (in 16-bit mode)
--
-- Uses little-endian.
writeReal16 :: RealAddress -> Word16 -> GBA s ()
writeReal16 (seg, off) val = do
    writeReal8 (seg, off) . fromIntegral $ shiftR (shiftL val 8) 8
    writeReal8 (seg, off+1) . fromIntegral $ shiftR val 8

-- | Write to a real address. (in 32-bit mode)
--
-- Uses little-endian.
writeReal32 :: RealAddress -> Word32 -> GBA s ()
writeReal32 (seg, off) val = do
    writeReal16 (seg, off) . fromIntegral $ shiftR (shiftL val 16) 16
    writeReal16 (seg, off+2) . fromIntegral $ shiftR val 16

-- | Read from a real address. (in 8-bit mode)
readReal8 :: RealAddress -> GBA s Word8
readReal8 addr = do
    mem <- view $ gbaMemoryMap
    liftBase $ readRaw mem addr

-- | Read from a real address. (in 16-bit mode)
--
-- Uses little-endian.
readReal16 :: RealAddress -> GBA s Word16
readReal16 (seg, off) = do
    ms <- fromIntegral <$> readReal8 (seg, off+1)
    ls <- fromIntegral <$> readReal8 (seg, off)
    return $ shiftL ms 8 `xor` ls

-- | Read from a real address. (in 32-bit mode)
--
-- Uses little-endian.
readReal32 :: RealAddress -> GBA s Word32
readReal32 (seg, off) = do
    ms <- fromIntegral <$> readReal16 (seg, off+2)
    ls <- fromIntegral <$> readReal16 (seg, off)
    trace (show ms) . trace (show ls) . return $ shiftL ms 16 `xor` ls
