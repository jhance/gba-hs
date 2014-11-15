module Game.GBA.CPUFlag
    ( ConditionFlag(..)
    , ControlFlag(..)
    , ProcessorMode(..)
    , getCondition
    , setCondition
    , setConditions
    , setModeRaw
    , getMode
    )
where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Game.GBA.Monad

-- | NZCVQ, in that order.
data ConditionFlag = CFSign | CFZero | CFCarry | CFOverflow | CFStickyCarry
    deriving (Enum, Ord, Eq, Show, Read)

-- | IFT, in that order
data ControlFlag = CFDisableIRQ | CFDisableFIQ | CFState

data ProcessorMode = ARM | Thumb
    deriving (Enum, Ord, Eq, Show, Read)

conditionIndex :: ConditionFlag -> Int
conditionIndex CFSign = 31
conditionIndex CFZero = 30
conditionIndex CFCarry = 29
conditionIndex CFOverflow = 28
conditionIndex CFStickyCarry = 27

getCondition :: ConditionFlag -> GBA s Bool
getCondition flag = flip testBit (conditionIndex flag) <$> readCPSR

setCondition :: ConditionFlag -> Bool -> GBA s ()
setCondition flag k = withCPSR $ \t -> if k 
    then setBit t (conditionIndex flag)
    else clearBit t (conditionIndex flag)

setConditions :: [(ConditionFlag, Bool)] -> GBA s ()
setConditions = mapM_ $ uncurry setCondition

setModeRaw :: ProcessorMode -> GBA s ()
setModeRaw mode = withCPSR $ \t -> if fromEnum mode == 1
    then setBit t 5
    else clearBit t 5

getMode :: GBA s ProcessorMode
getMode = toEnum . fromEnum . flip testBit 5 <$> readCPSR