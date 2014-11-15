{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module contains essentially the
-- most primitive items which rely on the
-- ghc monad.
module Game.GBA.Monad
    ( GBA
    , GBAContext
    , makeGBAContext
    , runGBA
    -- * General context lens operations
    , readLens
    , writeLens
    -- * Internal lenses
    , gbaRegisters
    , gbaMemoryMap
    )
where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Bits
import           Data.Word
import           Data.STRef

import           Game.GBA.RegisterSet
import           Game.GBA.MemoryMap

data GBAContext s = GBAContext {
        _gbaRegisters :: RegisterSet s
      , _gbaMemoryMap :: MemoryMap s
    }

makeLenses ''GBAContext

-- | Creates an empty context.
makeGBAContext :: ST s (GBAContext s)
makeGBAContext = GBAContext <$> makeRegisterSet <*> makeMemoryMap

readLens :: Lens' (GBAContext s) (STRef s a) -> GBA s a
readLens lens = view lens >>= liftBase . readSTRef

writeLens :: Lens' (GBAContext s) (STRef s a) -> a -> GBA s ()
writeLens lens val = view lens >>= liftBase . flip writeSTRef val

newtype GBA s a = GBA (ReaderT (GBAContext s) (ST s) a)
    deriving (Functor, Applicative, Monad, MonadReader (GBAContext s),
              MonadBase (ST s))

runGBA :: GBAContext s -> GBA s a -> ST s a
runGBA context (GBA gba) = runReaderT gba context

--type GBA s a = ReaderT (GBAContext s) (ST s) a
