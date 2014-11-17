{-# LANGUAGE QuasiQuotes #-}
module Main where

import Criterion.Main

import Control.Monad
import Control.Monad.ST
import Language.Literals.Binary

import Game.GBA.Boot
import Game.GBA.Monad
import Game.GBA.Register
import Game.GBA.Thumb.Instruction
import Game.GBA.Thumb.Execution

main :: IO ()
main = main1

main1 = do
    context <- stToIO $ makeGBAContext
    stToIO $ runGBA context (bootForTest Thumb)
    -- singleton list
    defaultMain . return . bench "shift register lsl" . whnfIO . stToIO . runGBA context $ do
        writeRegister [b|000|] 57
        execute $ TSR TSRO_LSL 1 0 1
        readRegister [b|001|]

main2 = do
    context <- stToIO $ makeGBAContext
    stToIO $ runGBA context (bootForTest Thumb)
    -- singleton list
    replicateM_ 10000 . stToIO . runGBA context $ do
        writeRegister [b|000|] 57
        execute $ TSR TSRO_LSL 1 0 1
        readRegister [b|001|]