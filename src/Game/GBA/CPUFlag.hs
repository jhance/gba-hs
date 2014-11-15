module Game.GBA.CPUFlag
    (
    )
where

import           Control.Applicative
import           Data.Bits
import           Game.GBA.Monad

-- | NZCVQ, in that order.
data ConditionFlag = CFSign | CFZero | CFCarry | CFOverflow | CFStickyCarry
    deriving (Enum, Ord, Eq, Show, Read)

-- | IFT, in that order
data ControlFlag = CFDisableIRQ | CFDisableFIQ | CFState

conditionIndex :: ConditionFlag -> Int
conditionIndex CFSign = 31
conditionIndex CFZero = 30
conditionIndex CFCarry = 29
conditionIndex CFOverflow = 28
conditionIndex CFStickyCarry = 27

getControl :: ConditionFlag -> GBA s Bool
getControl flag = flip testBit (conditionIndex flag) <$> readCPSR

setControl :: ConditionFlag -> Bool -> GBA s ()
setControl flag k = withCPSR $ \t -> if k 
    then setBit t (conditionIndex flag)
    else clearBit t (conditionIndex flag)
