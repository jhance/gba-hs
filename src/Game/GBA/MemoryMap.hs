{-# LANGUAGE TemplateHaskell #-}
module Game.GBA.MemoryMap
    ( MemoryMap
    , VirtualAddress
    -- * Internals
    , RealAddress
    , Segment
    , SegmentOffset
    , virtual
    , makeMemoryMap
    , readRaw
    , writeRaw
    )
where

import           Control.Applicative
import           Control.Monad.ST
import           Data.Word
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector.Mutable as MV
import           Numeric (showHex)

-- | Represents a memory mapping for *actual memory*.
-- What this means is that some virtual address reads/writes
-- will not actually go through this api.
newtype MemoryMap s = MemoryMap {unMemoryMap :: MV.MVector s (MUV.MVector s Word8)}

-- | Addresses according to us.
type RealAddress = (Segment, SegmentOffset)

-- | Full list of memory segments: (104 total)
--
-- General Internal Memory
-- [0] 0000 0000 - 0000 3FFF - BIOS (16 kb)
-- [1] 0200 0000 - 0203 FFFF - WRAM on board (256 kb)
-- [2] 0300 0000 - 0300 7FFF - WRAM on chip (32 kb)
-- [3] 0400 0000 - 0400 03FE - I/O registers
--
-- Internal display memory
-- [4] 0500 0000 - 0500 03FF - BJ/OBJ (1 kb)
-- [5] 0600 0000 - 0601 7FFF - VRAM (96 kb)
-- [6]
-- [7+k], 0 <= k <= 95,
--   0800 0000 + k(1 MB), 0810 000 + k(1 MB)
-- is k'th row of the 96 MB of Game Pak ROM
-- Note: 1M = 0x100000
-- [103] 0E00 0000 - 0E00 FFFF Game Pak SRAM (64 kb)
type Segment = Int

type SegmentOffset = Int

-- | Addresses in GBA land.
type VirtualAddress = Word32

-- | In interval.
ini :: VirtualAddress -> (VirtualAddress, VirtualAddress) -> Bool
ini x (a, b) = x >= a && x <= b

-- | Used to access the memory map from a virtual memory address.
-- Not total. Assumes that the address is valid.
virtual :: VirtualAddress -> RealAddress
virtual x
    | x `ini` (0x00000000, 0x00003FFF) = (0, fromIntegral $ x - 0x00000000)
    | x `ini` (0x02000000, 0x0203FFFF) = (1, fromIntegral $ x - 0x02000000)
    | x `ini` (0x03000000, 0x03007FFF) = (2, fromIntegral $ x - 0x03000000)
    | x `ini` (0x04000000, 0x040003FE) = (3, fromIntegral $ x - 0x04000000)
    | x `ini` (0x05000000, 0x050003FF) = (4, fromIntegral $ x - 0x05000000)
    | x `ini` (0x06000000, 0x06017FFF) = (5, fromIntegral $ x - 0x06000000)
    | x `ini` (0x07000000, 0x070003FF) = (6, fromIntegral $ x - 0x07000000)
    | x `ini` (0x0E000000, 0x0E00FFFF) = (103, fromIntegral $ x - 0x0E000000)
    | x `ini` (0x08000000, 0x0DFFFFFF) = -- 7 through 102
        let k = fromIntegral $ (x - 0x080000000) `div` 0x10000
        in (7 + k, fromIntegral x - 0x08000000 + 0x100000 * k)
    | otherwise = error $ "segment fault (invalid virtual address): 0x" ++ showHex x ""

writeRaw :: MemoryMap s -> RealAddress -> Word8 -> ST s ()
writeRaw (MemoryMap mem) (seg, off) val = do
    if seg >= MV.length mem
        then error $ "segment fault (invalid segment): " ++ show seg
        else do 
            segv <- MV.read mem seg
            if off >= MUV.length segv
                then error $ "segment fault (invalid offset in segment " ++ show seg
                                ++ "): 0x" ++ showHex off ""
                else MUV.write segv off val

readRaw :: MemoryMap s -> RealAddress -> ST s Word8
readRaw (MemoryMap mem) (seg, off) = do
    if seg >= MV.length mem
        then error $ "segment fault (invalid segment): " ++ show seg
        else do 
            segv <- MV.read mem seg
            if off >= MUV.length segv
                then error $ "segment fault (invalid offset in segment " ++ show seg
                                ++ "): 0x" ++ showHex off ""
                else MUV.read segv off

makeMemoryMap :: ST s (MemoryMap s)
makeMemoryMap = MemoryMap <$> do
    mem <- MV.new 104
    mapM_ (\(index, size) -> MUV.new size >>= MV.write mem index) $
        [ (0, 0x00004000)
        , (1, 0x00040000)
        , (2, 0x00008000)
        , (3, 0x000003FF)
        , (4, 0x00000400)
        , (5, 0x00018000)
        , (6, 0x00000400)
        , (103, 0x00010000)
        ]
    mapM_ (\index -> MUV.new 0x100000 >>= MV.write mem index) $ [7..102]
    return mem
