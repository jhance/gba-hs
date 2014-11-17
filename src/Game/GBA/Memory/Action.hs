{-# LANGUAGE TemplateHaskell #-}
-- | Abstract virtual memory actions (ie, actions that don't correspond
-- to just writing to some @RealAddress@).
module Game.GBA.Memory.Action
    ( MemoryReadAction(..)
    , MemoryWriteAction(..)
    , MemoryReadMap
    , MemoryWriteMap
    -- * Utilities
    , readOnly8
    , readOnly16
    , readOnly32
    , writeOnly8
    , writeOnly16
    , writeOnly32
    )
where

import           Data.Word
import qualified Data.Map as M
import           Game.GBA.MemoryMap
import           Game.GBA.Monad

-- | Abstract read-memory action.
--
-- This is an address for which reading from a @VirtualAddress@
-- should correspond to some action rather than a load from
-- real memory.
--
-- If any of the actions are @Nothing@, an error (or exception)
-- will be thrown upon trying to write with that bus size.
data MemoryReadAction s = MemoryReadAction {
      readAction8 :: Maybe (GBA s Word8)
    , readAction16 :: Maybe (GBA s Word16)
    , readAction32 :: Maybe (GBA s Word32)
    }

type MemoryReadMap s = M.Map Word32 (MemoryReadAction s)

readOnly8 :: VirtualAddress -> GBA s Word8 -> MemoryReadMap s
readOnly8 addr act = M.singleton addr $ MemoryReadAction (Just act) Nothing Nothing

readOnly16 :: VirtualAddress -> GBA s Word16 -> MemoryReadMap s
readOnly16 addr act = M.singleton addr $ MemoryReadAction Nothing (Just act) Nothing

readOnly32 :: VirtualAddress -> GBA s Word32 -> MemoryReadMap s
readOnly32 addr act = M.singleton addr $ MemoryReadAction Nothing Nothing (Just act)

-- | Abstract write-memory action.
--
-- This is an address for which writing from a @VirtualAddress@
-- should correspond to some action rather than a write
-- to a @RealAddress@.
--
-- If any of the actions are @Nothing@, an error (or exception)
-- will be thrown upon trying to write with that bus size.
data MemoryWriteAction s = MemoryWriteAction {
      writeAction8 :: Maybe (Word8 -> GBA s ())
    , writeAction16 :: Maybe (Word16 -> GBA s ())
    , writeAction32 :: Maybe (Word32 -> GBA s ())
    }

type MemoryWriteMap s = M.Map Word32 (MemoryWriteAction s)

writeOnly8 :: VirtualAddress -> (Word8 -> GBA s ()) -> MemoryWriteMap s
writeOnly8 addr act = M.singleton addr $ MemoryWriteAction (Just act) Nothing Nothing

writeOnly16 :: VirtualAddress -> (Word16 -> GBA s ()) -> MemoryWriteMap s
writeOnly16 addr act = M.singleton addr $ MemoryWriteAction Nothing (Just act) Nothing

writeOnly32 :: VirtualAddress -> (Word32 -> GBA s ()) -> MemoryWriteMap s
writeOnly32 addr act = M.singleton addr $ MemoryWriteAction Nothing Nothing (Just act)