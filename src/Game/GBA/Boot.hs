module Game.GBA.Boot
where

import Game.GBA.Monad
import Game.GBA.Register

bootForTest :: ProcessorMode -> GBA s ()
bootForTest mode = do
    writeStatus statusB UserMode
    writeStatus statusT mode
