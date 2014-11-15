-- | Abstract memory operations.
-- 
-- If you are looking for internals, see @MemoryMap@.
module Game.GBA.Memory
    ( VirtualAddress
    , writeVirtual
    , readVirtual
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

-- | Internal: write to a real address.
-- I'm undecided on whether or not to export this.
writeReal :: RealAddress -> Word8 -> GBA s ()
writeReal (seg, off) val = do
    mem <- view $ gbaMemoryMap . unMemoryMap
    liftBase $ do
        segv <- MV.read mem seg
        MUV.write segv off val

-- | Internal: read from a real address.
-- I'm undecided on whether or not to export this.
readReal :: RealAddress -> GBA s Word8
readReal (seg, off) = do
    mem <- view $ gbaMemoryMap . unMemoryMap
    liftBase $ do
        segv <- MV.read mem seg
        MUV.read segv off

-- | Write to a virtual address.
writeVirtual :: VirtualAddress -> Word8 -> GBA s ()
writeVirtual addr val = writeReal (virtual addr) val

-- | Read from a virtual address.
readVirtual :: VirtualAddress -> GBA s Word8
readVirtual addr = readReal (virtual addr)