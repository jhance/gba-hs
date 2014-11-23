module Game.GBA.Thumb.Execution
    (execute)
where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Word
import           Game.GBA.Thumb.Instruction
import           Game.GBA.Monad
import           Game.GBA.Register

setZero :: Word32 -> GBA s ()
setZero val = writeStatus statusZ $ val == 0

setSign :: Word32 -> GBA s ()
setSign val = writeStatus statusN $ testBit val 31

-- t1
-----
executeT1 :: T1Opcode -> Word8 -> RegisterID -> RegisterID -> GBA s ()
executeT1 T1_LSL 0 src dest = do
    val <- readSafeRegister src
    writeSafeRegister dest val
    setSign val
    setZero val

executeT1 _ 0 _ _ = error "invalid t1; should through exception!"
executeT1 T1_LSL n src dest = do
    val <- readSafeRegister src
    let val' = shiftL val (fromIntegral n)
    writeSafeRegister dest val'
    writeStatus statusC $ testBit val 31
    setZero val'
    setSign val'
executeT1 T1_LSR n src dest = do
    val <- readRegister src
    let val' = shiftR val (fromIntegral n)
    writeRegister dest val'
    writeStatus statusC $ testBit val (fromIntegral n - 1)
    setZero val'
    setSign val'
executeT1 T1_ASR n src dest = do
    let n' = fromIntegral n
    val <- readRegister src
    let val' = shiftR val n'
        val'' = if testBit val 31
                    -- set left-most n bits to 1
                    then shiftL (complement 0) (32 - n') + val'
                    else val'
    writeRegister dest val''
    writeStatus statusC $ testBit val (n' - 1)
    setZero val''
    setSign val''

-- | Gets either the register or the direct number
-- for a T2.

-- t2
-----
-- needs refactor
executeT2 :: T2OperandType -> T2Operation -> Word8
          -> RegisterID -> RegisterID -> GBA s ()
executeT2 otype atype operand src dest = do
    operand' <- tasConvert atype <$> tasValue otype operand
    val <- readRegister src
    let sameSign = testBit val 31 == testBit operand' 31
        result = val + operand'
    writeRegister dest $ val + operand'
    setZero result
    setSign result
    writeStatus statusC $ result < val
    writeStatus statusV $ sameSign && testBit val 31 /= testBit result 31

tasValue :: T2OperandType -> Word8 -> GBA s Word32
tasValue T2_REG = readRegister . fromIntegral
tasValue T2_NUM = return . fromIntegral

tasConvert :: T2Operation -> Word32 -> Word32
tasConvert T2_ADD x = x
tasConvert T2_SUB x = complement x + 1

-- t3
-----
executeT3 :: T3Opcode -> RegisterID -> Word32 -> GBA s ()
executeT3 T3_MOV reg num = do
    writeSafeRegister reg num
    setZero num
    -- num is 8 bit unsigned - it can't have a sign bit = 1.
    writeStatus statusN False
executeT3 T3_ADD reg num = do
    val <- readSafeRegister reg
    let result = val + num
        pos = not $ testBit val 31
    setZero result
    setSign result
    writeStatus statusC $ result < val
    writeStatus statusV $ pos && testBit result 31
    writeSafeRegister reg result
executeT3 opcode reg num = do -- cmp, sub
    val <- readSafeRegister reg
    let result = val - num
        neg = testBit val 31
    setZero result
    setSign result
    writeStatus statusC $ val >= num
    writeStatus statusV $ neg && not (testBit result 31)
    when (opcode ==  T3_SUB) $ writeSafeRegister reg result

-- (warning: long, 16 cases for opcode...)
-- t4
-----
executeT4 :: T4Opcode -> RegisterID -> RegisterID -> GBA s ()
executeT4 T4_AND src dest = do
    in1 <- readSafeRegister src
    in2 <- readSafeRegister dest
    let result = in1 .&. in2
    setZero result
    setSign result
    writeSafeRegister dest result
executeT4 T4_EOR src dest = do
    in1 <- readSafeRegister src
    in2 <- readSafeRegister dest
    let result = in1 `xor` in2
    setZero result
    setSign result
    writeSafeRegister dest result
executeT4 T4_ADC src dest = do
    in1 <- readSafeRegister src
    in2 <- readSafeRegister dest
    c <- fromIntegral . fromEnum <$> readStatus statusC
    let result = in1 + in2 + c
        sign = testBit in1 31
        sameSign = sign == testBit in2 31
    setZero result
    setSign result
    writeStatus statusC $ result < in1 || (result == in1 && c /= 0)
    writeStatus statusV $ sameSign && sign /= testBit result 31
    writeSafeRegister dest result
executeT4 T4_SBC src dest = do
    in1 <- readSafeRegister src
    in2 <- readSafeRegister dest
    c <- fromIntegral . fromEnum . not <$> readStatus statusC
    let result = in1 - in2 - c
        sign = testBit in1 31
        difSign = sign /= testBit in2 31
    setZero result
    setSign result
    writeStatus statusC $ in1 >= in2 + c
    writeStatus statusV $ difSign && sign /= testBit result 31
    writeSafeRegister dest result

-- | Execution of any Thumb mode instruction.
-- Please do not try to execute outside of Thumb mode.
-- There is no check.
execute :: TInstruction -> GBA s ()
execute (T1 opcode offset src dest) =
    executeT1 opcode offset src dest
execute (T2 srcType opType operand source dest) =
    executeT2 srcType opType operand source dest
execute (T3 opcode dest num) =
    executeT3 opcode dest num
execute (T4 opcode src dest) =
    executeT4 opcode src dest
