{-# LANGUAGE DeriveGeneric #-}
module Instruction (
    SimpleInstruction(..),
    FloatInstruction(..),
    IntInstruction(..),
    Instruction(..),

    instructionNames,
    instruction,
    instructionName,
    readInstruction
) where
import Data.Int
import Data.Binary
import Data.Tuple (swap)
import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Data.List (genericLength)
import Control.Applicative (liftA, liftA2)
import Text.Read (readMaybe)


data SimpleInstruction = NOP
                       | NEWOBJECT
                       | GETFIELD
                       | SETFIELD
                       | ADD
                       | CALLI --Calls the function on the top of the stack
                       | SUBTRACT
                       | MULTIPLY
                       | DIVIDE
                       | REMAINDER
                       deriving (Eq, Show, Read, Enum)


data IntInstruction = MOVE
                    | LOADI
                    | LOADSTR
                    | CALL
                    deriving (Eq, Show, Read, Enum)

data FloatInstruction = LOADF deriving (Eq, Show, Read, Enum)

data Instruction = IntInstruction IntInstruction Int32
                 | FloatInstruction FloatInstruction Double
                 | SimpleInstruction SimpleInstruction
                 deriving (Eq, Show, Read)

getInstruction table = do
    w <- get :: Get Word8
    maybeToMonad (show w ++ " is not a valid instruction for this type") $ lookup w table

setInstruction table op = do
    w <- maybeToMonad "" $ lookup op (map swap table)
    put w

simpleInstructions = [NOP .. REMAINDER]
floatInstructions = [LOADF]
intInstructions = [MOVE .. CALL]

class Opcode o where
    instruction :: Integral i => o -> i -> Instruction

    instructions :: [o]

instance Opcode SimpleInstruction where
    instruction op _ = SimpleInstruction op
    
    instructions = simpleInstructions

instance Opcode FloatInstruction where
    instruction op i = FloatInstruction op $ fromIntegral i
    
    instructions = floatInstructions

instance Opcode IntInstruction where
    instruction op i = IntInstruction op $ fromIntegral i

    instructions = intInstructions

instance Binary SimpleInstruction where
    get = getInstruction simpleILookup
    put = setInstruction simpleILookup

instance Binary FloatInstruction where
    get = getInstruction floatILookup
    put = setInstruction floatILookup

instance Binary IntInstruction where
    get = getInstruction intILookup
    put = setInstruction intILookup

instance Binary Instruction where
    get = liftA SimpleInstruction get <|> liftA2 FloatInstruction get get <|> liftA2 IntInstruction get get

    put (SimpleInstruction i) = do
        let Just w = lookup i $ map swap simpleILookup
        put w
    put (FloatInstruction i f) = do
        let Just w = lookup i $ map swap floatILookup
        put w
        put f
    put (IntInstruction instr intValue) = do
        let Just w = lookup instr $ map swap intILookup
        put w
        put intValue

instructionName :: Instruction -> String
instructionName (IntInstruction i _) = show i
instructionName (FloatInstruction i _) = show i
instructionName (SimpleInstruction i) = show i


instructionNames = map show simpleInstructions ++ map show floatInstructions ++ map show intInstructions

simpleILookup = zip [0..] simpleInstructions
floatILookup = zip [genericLength simpleILookup..] floatInstructions
intILookup = zip [genericLength floatILookup..] intInstructions          

maybeToMonad _ (Just x) = return x
maybeToMonad msg Nothing = fail msg

readInstruction :: Integral i => String -> i -> Maybe Instruction
readInstruction str i = (SimpleInstruction <$> readMaybe str) <|> (FloatInstruction <$> readMaybe str <*> pure (fromIntegral i)) <|> (IntInstruction <$> readMaybe str <*> pure (fromIntegral i))
