module Game.GBA.Thumb.Execution
    (execute)
where

import           Data.Word
import           Game.GBA.Thumb.Instruction
import           Game.GBA.Monad

-- | Gets either the register or the direct number
-- for a TAS.
tasValue :: TASOperandType -> Word8 -> GBA s Word32
tasValue TASO_REG = readRegister . fromIntegral
tasValue TASO_NUM = return . fromIntegral

tasOperation :: TASOperation -> Word32 -> Word32 -> Word32
tasOperation TASO_ADD = (+)
tasOperation TASO_SUB = (-)

-- | This is a big function! And really important!!
execute :: TInstruction -> GBA s ()

-- t2
execute (TAS srcType opType operand source dest) = do
    operand' <- tasValue srcType operand
    value <- readRegister source
    let result = tasOperation opType value operand'
    writeRegister dest result
    -- TODO CPU FLAGS