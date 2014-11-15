module Game.GBA.Boot
where

import Game.GBA.CPUFlag
import Game.GBA.Monad
import Game.GBA.Register

bootForTest :: ProcessorMode -> GBA s ()
bootForTest mode = do
    setBankMode UserMode
    setModeRaw mode