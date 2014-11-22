{-# LANGUAGE RankNTypes #-}
module Test.GBA.Common
    ( runTest
    , runPure
    , ThumbRegister(..)
    )
where

import Game.GBA.Boot
import Game.GBA.Monad
import Game.GBA.Register

import Control.Applicative
import Control.Monad.ST

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers

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
