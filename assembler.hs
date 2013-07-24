{-# LANGUAGE FlexibleContexts #-}
module Assembler where
import Text.ParserCombinators.Parsec hiding (GenParser, try)
import Data.Binary (encode, put)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment (getArgs)
import System.IO (IOMode(..), withFile)
import Control.Monad.State (StateT(..), modify, gets)
import Control.Monad.Identity
import Text.Parsec
import qualified Data.Map as Map
import Data.Int
import Instruction
import Disassemble
import Expression
import qualified Data.Vector as V

data ParseState = ParseState {
    offset :: Int32,
    assembly :: Assembly
    }

type StateParse a = ParsecT B.ByteString () (StateT ParseState Identity) a

bString :: Stream s m Char => String -> ParsecT s u m B.ByteString
bString xs = string xs >>= return . B.pack

eol :: Stream s m Char => ParsecT s u m B.ByteString
eol =   try (bString "\n\r")
    <|> try (bString "\r\n")
    <|> bString "\n"
    <|> bString "\r"

matchOne :: Stream s m Char => [String] -> ParsecT s u m String
matchOne [] = fail "Could not find a match"
matchOne (x:xs) = string x <|> matchOne xs

readMaybe        :: (Read a) => String -> Maybe a
readMaybe s      =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> Just x
                         _   -> Nothing

instruction :: StateParse B.ByteString
instruction = do
    keyword <- matchOne keywords
    instr <- case readMaybe keyword of
        Nothing -> fail $ keyword ++ " is not a valid instruction"
        Just i -> return i
    many1 space
    numbers <- sepBy1 (many1 digit) (char ',')
    let i = case numbers of
            [] -> error ""
            [x] -> Instruction instr (read x) 0 0
            [x,y] -> Instruction instr (read x) (read y) 0
            (x:y:z:_) -> Instruction instr (read x) (read y) (read z)
    let binary = runPut (put i)
    modify $ \st -> st { offset = offset st + 1 }
    return binary

asmLabels = ["entrypoint", "method"]

modifyAssembly f = modify $ \st -> st { assembly = f (assembly st) }
modifyEntrypoint entry = modifyAssembly $ \asm -> asm { entrypoint = entry }

doLabel "entrypoint" = do
    entry <- gets offset
    modifyEntrypoint (fromIntegral entry)
doLabel "method" = do
    many1 space
    name <- many1 alphaNum
    currentOffset <- gets offset
    --set the current offset as being the start of a method
    modifyAssembly $ \asm -> asm { methods = Map.insert name currentOffset (methods asm) } 
doLabel label = fail $ "Unimplmented label " ++ label

asmLabel :: StateParse B.ByteString
asmLabel = do
    char '.'
    label <- matchOne asmLabels
    doLabel label
    return $ B.pack ""


assemblyFile = do
    let line = asmLabel <|> instruction 
    lines <- sepEndBy line (many1 eol)
    eof
    return $ B.concat lines



runStateParse ::
    Stream s1 (StateT s Identity) t =>
    ParsecT s1 () (StateT s Identity) a ->
    SourceName -> s1 -> s -> (Either ParseError a, s)
runStateParse parser sourceName input state = runIdentity $ runStateT runParse state
    where
        runParse = runPT parser () sourceName input

run parser onSuccess filename = do
    withFile filename ReadMode $ \handle -> do
        contents <- B.hGetContents handle
        let startState = ParseState 0 (Assembly 0 Map.empty [])
            (parseResult, endState) = runStateParse parser filename contents startState
        case parseResult of
            Left err -> putStrLn $ show err 
            Right str -> do
                putStrLn $ show $ B.length str
                onSuccess str endState

writeAssemblyFile name code (ParseState _ (Assembly entry _ _)) = do
    B.writeFile name output
    where
        output = header `B.append` code
        header = encode entry --`B.append` encode methods

main = do
    x:xs <- getArgs
    let filename = if x == "-d" then head xs else x
    let baseFilename = takeWhile (/='.') filename
    if x == "-d" then
        disassemble filename (baseFilename ++ ".disasm")
    else
        run assemblyFile (writeAssemblyFile (baseFilename ++ ".asm")) filename


data Identifier = Local Int
                | Global String
    deriving(Eq)


data Data = StringData String
          | FunctionData String

data Environment = Environment {
    modul :: Module,
    stack :: V.Vector String,
    dataIdentifiers :: V.Vector Data
    }

type CompileState = StateT Environment Identity

--tries to find a identifier in the state
lookupIdentifier :: String -> CompileState (Maybe Identifier)
lookupIdentifier name = liftM2 mplus (gets local) (gets global)
    where
        --Use of mplus acts as a short circuit for Maybe
        local = liftM Local . V.elemIndex name . stack
        global env = do
            let funcs = functions $ modul env
            FunctionDefinition name _ _ <- Map.lookup name funcs
            return $ Global name

addData :: Data -> CompileState Int32
addData d = do
    modify $ \env -> env { dataIdentifiers = V.snoc (dataIdentifiers env) d }
    i <- gets (V.length . dataIdentifiers)
    return $ fromIntegral i
    

compileCall :: String -> [Expr] -> CompileState [Instruction]
compileCall name [l,r] =
    case arithInstruction name of
        Just instructionName -> do
            args <- liftM2 (++) (compile l) (compile r)
            return $ Instruction instructionName 0 0 0 : args
        Nothing -> compileCall name [l,r]
compileCall name xs = do
    maybeId <- lookupIdentifier name
    instr <- case maybeId of
        Nothing ->  fail $ "Could not find identifier " ++ name
        Just ident -> f ident
    args <- (mapM compile xs)
    return $ instr ++ concat args
    where
        f (Local index) = return [Instruction MOVE (fromIntegral index) 0 0]
        f (Global ident) = do
            dataIndex <- addData (FunctionData ident)
            return [Instruction CALL (fromIntegral dataIndex) 0 0]
    

arithInstruction op = lookup op [("+", ADD), ("-", SUBTRACT), ("*", MULTIPLY), ("/", DIVIDE)]

compile :: Expr -> CompileState [Instruction]
compile (IntegerValue i) = return [Instruction LOADI (fromIntegral i) 0 0]
compile (StringLiteral str) = do
    index <- addData (StringData str)
    return [Instruction LOADSTR index 0 0]
compile (Call name xs) = compileCall name xs
compile _ = undefined


compileFunction :: FunctionDefinition -> CompileState [Instruction]
compileFunction (FunctionDefinition name args expr) = do
    modify $ \env -> env { stack = V.fromList args }
    compile expr
    
