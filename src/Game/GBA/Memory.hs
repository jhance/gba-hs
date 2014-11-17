-- | Abstract memory operations.
-- 
-- If you are looking for internals on the memory
-- mapping itself, see @MemoryMap@.
module Game.GBA.Memory
    ( VirtualAddress
    , writeVirtual8
    , writeVirtual16
    , writeVirtual32
    , readVirtual8
    , readVirtual16
    , readVirtual32
    )
where

import           Control.Monad.Base
import           Control.Monad.ST
import qualified Data.Map as M
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MUV
import           Data.Word
import           Game.GBA.MemoryMap
import           Game.GBA.Monad
import           Game.GBA.Memory.Action
import           Game.GBA.Memory.Real
import           Numeric (showHex)

writeMap :: MemoryWriteMap s
writeMap = M.unions
    [ writeOnly16 0x04000000 $ writeLens timerReload0
    , writeOnly16 0x04000002 $ writeLens timerReload1
    , writeOnly16 0x04000004 $ writeLens timerReload2
    , writeOnly16 0x04000006 $ writeLens timerReload3
    ]

readMap :: MemoryReadMap s
readMap = M.empty

-- | Write to a virtual address. (in 8-bit mode)
writeVirtual8 :: VirtualAddress -> Word8 -> GBA s ()
writeVirtual8 addr = case M.lookup addr writeMap of
    Nothing -> writeReal8 $ virtual addr
    Just k -> case writeAction8 k of
        Nothing -> const $
            error $ "memory fault: illegal 8-bit write at 0x" ++ showHex addr ""
        Just f -> f

-- | Write to a virtual address. (in 16-bit mode)
writeVirtual16 :: VirtualAddress -> Word16 -> GBA s ()
writeVirtual16 addr = case M.lookup addr writeMap of
    Nothing -> writeReal16 $ virtual addr
    Just k -> case writeAction16 k of
        Nothing -> const $
            error $ "memory fault: illegal 16-bit write at 0x" ++ showHex addr ""
        Just f -> f

-- | Write to a virtual address. (in 32-bit mode)
writeVirtual32 :: VirtualAddress -> Word32 -> GBA s ()
writeVirtual32 addr = case M.lookup addr writeMap of
    Nothing -> writeReal32 $ virtual addr
    Just k -> case writeAction32 k of
        Nothing -> const $
            error $ "memory fault: illegal 32-bit write at 0x" ++ showHex addr ""
        Just f -> f

-- | Read from a virtual address. (in 8-bit mode)
readVirtual8 :: VirtualAddress -> GBA s Word8
readVirtual8 addr = case M.lookup addr readMap of
    Nothing -> readReal8 $ virtual addr
    Just k -> case readAction8 k of
        Nothing -> error $ "memory fault: illegal 8-bit read at 0x" ++ showHex addr ""
        Just f -> f

-- | Read from a virtual address. (in 16-bit mode)
readVirtual16 :: VirtualAddress -> GBA s Word16
readVirtual16 addr = case M.lookup addr readMap of
    Nothing -> readReal16 $ virtual addr
    Just k -> case readAction16 k of
        Nothing -> error $ "memory fault: illegal 16-bit read at 0x" ++ showHex addr ""
        Just f -> f

-- | Read from a virtual address. (in 32-bit mode)
readVirtual32 :: VirtualAddress -> GBA s Word32
readVirtual32 addr = case M.lookup addr readMap of
    Nothing -> readReal32 $ virtual addr
    Just k -> case readAction32 k of
        Nothing -> error $ "memory fault: illegal 32-bit read at 0x" ++ showHex addr ""
        Just f -> f
