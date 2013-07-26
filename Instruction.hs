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

readBinaryInstruction :: Word8 -> Get Instruction
readBinaryInstruction iBinary = try simpleWrapper simpleILookup <|> try FloatInstruction floatILookup <|> try IntInstruction intILookup
    where
        try :: Binary a => (t -> a -> Instruction) -> [(Word8, t)] -> Get Instruction
        try ctor lookupTable = do
            instruction <- maybeToGet (lookup iBinary lookupTable)
            i <- get
            return $ ctor instruction i
        maybeToGet (Just x) = return x
        maybeToGet Nothing = fail "Got Nothing in maybeToGet"
        --Use unit as the second argument to ignore the get action
        simpleWrapper i () = SimpleInstruction i
            
instance Binary Instruction where
    get = get >>= readBinaryInstruction

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


instruction :: Integral a => Word8 -> a -> Maybe Instruction
instruction w i =
    (fmap IntInstruction (lookupI intILookup) <*> (pure $ fromIntegral i)) <|>
    (fmap FloatInstruction (lookupI floatILookup) <*> (pure $ fromIntegral i)) <|>
    (fmap SimpleInstruction (lookupI simpleILookup))
    where
        lookupI = lookup w
