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

instruction :: StateParse B.ByteString
instruction = do
    keyword <- matchOne keywords
    many1 space
    numbers <- sepBy1 (many1 digit) (char ',')
    let i = case numbers of
            [] -> error ""
            [x] -> Instruction keyword (read x) 0 0
            [x,y] -> Instruction keyword (read x) (read y) 0
            (x:y:z:_) -> Instruction keyword (read x) (read y) (read z)
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

writeAssemblyFile name code (ParseState _ (Assembly entry methods _)) = do
    putStrLn $ show $ B.length code
    putStrLn $ show $ B.length header
    putStrLn $ show $ B.length output
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

