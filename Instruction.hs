{-# LANGUAGE DeriveGeneric #-}
module Instruction (
    SimpleInstruction(..),
    FloatInstruction(..),
    IntInstruction(..),
    Instruction(..),

    instructionNames,
    instruction,
    instructionName
) where
import Data.Int
import Data.Binary
import Data.Tuple (swap)
import Control.Applicative ((<*>), (<|>), pure)
import Data.List (genericLength)
import Control.Applicative (liftA, liftA2)


data SimpleInstruction = NOP
                       | NEWOBJECT
                       | GETFIELD
                       | SETFIELD
                       | ADD
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

simpleInstructions = [NOP .. REMAINDER]
floatInstructions = [LOADF]
intInstructions = [MOVE .. CALL]

instructionNames = map show simpleInstructions ++ map show floatInstructions ++ map show intInstructions

simpleILookup = zip [0..] simpleInstructions
floatILookup = zip [genericLength simpleILookup..] floatInstructions
intILookup = zip [genericLength floatILookup..] intInstructions          

instruction :: Integral a => Word8 -> a -> Maybe Instruction
instruction w i =
    (fmap IntInstruction (lookupI intILookup) <*> (pure $ fromIntegral i)) <|>
    (fmap FloatInstruction (lookupI floatILookup) <*> (pure $ fromIntegral i)) <|>
    (fmap SimpleInstruction (lookupI simpleILookup))
    where
        lookupI = lookup w

maybeToMonad _ (Just x) = return x
maybeToMonad msg Nothing = fail msg
