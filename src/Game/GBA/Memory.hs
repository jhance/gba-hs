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

import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.ST
import           Data.Word
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MUV
import           Game.GBA.MemoryMap
import           Game.GBA.Monad
import           Game.GBA.Memory.Real

-- | Write to a virtual address. (in 8-bit mode)
writeVirtual8 :: VirtualAddress -> Word8 -> GBA s ()
writeVirtual8 addr = writeReal8 (virtual addr)

-- | Write to a virtual address. (in 16-bit mode)
writeVirtual16 :: VirtualAddress -> Word16 -> GBA s ()
writeVirtual16 addr = writeReal16 (virtual addr)

-- | Write to a virtual address. (in 16-bit mode)
writeVirtual32 :: VirtualAddress -> Word32 -> GBA s ()
writeVirtual32 addr = writeReal32 (virtual addr)

-- | Read from a virtual address. (in 8-bit mode)
readVirtual8 :: VirtualAddress -> GBA s Word8
readVirtual8 addr = readReal8 (virtual addr)

-- | Read from a virtual address. (in 16-bit mode)
readVirtual16 :: VirtualAddress -> GBA s Word16
readVirtual16 addr = readReal16 (virtual addr)

-- | Read from a virtual address. (in 32-bit mode)
readVirtual32 :: VirtualAddress -> GBA s Word32
readVirtual32 addr = readReal32 (virtual addr)