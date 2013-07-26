{-# LANGUAGE FlexibleContexts #-}
module Expression (
    Module(..),
    Literal(..),
    Expr(..),
    FunctionDefinition(..),

    parseExpr,
    parseFile
) where
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.Parsec.Language (haskell)
import Text.Parsec.Expr
import Text.Parsec
import Control.Monad.Identity
import Control.Applicative ((*>), (<*))
import qualified Control.Monad.State as S
import System.IO (IOMode(..), withFile, hGetContents)
import qualified Data.Map as Map

data PatternMatch = PatternMatch String [String] deriving (Eq, Show)

data Literal = Integer Integer
             | Float Double
             | StringLiteral String
             deriving (Eq, Show)

data Expr = Literal Literal
          | Var String
          | Apply Expr Expr
          | Lambda String Expr
          | Case Expr [(PatternMatch, Expr)]
          | Let [FunctionDefinition] Expr
          deriving (Eq, Show)

data FunctionDefinition = FunctionDefinition String [String] Expr deriving(Eq, Show)

data Constructor = Constructor String [String] deriving(Eq, Show)
data DataDefinition = DataDefinition String [Constructor] deriving(Eq, Show)

data Module = Module {
    functions :: Map.Map String FunctionDefinition,
    datas :: Map.Map String DataDefinition
    } deriving(Eq, Show)

type Parser u a = ParsecT String u Identity a
type HaskellParser u = Parser u Expr
type FileParser a = ParsecT String Module Identity a
--type HaskellParser u = T.TokenParser ()

applyArgs func args = foldl Apply func args

lexeme = T.lexeme haskell

identifier = T.identifier haskell <?> "Identifier"

integer :: Parser u Literal
integer = liftM Integer (T.integer haskell) <?> "Integerliteral"

float = liftM Float (T.float haskell) <?> "Floatliteral"

stringLiteral = liftM StringLiteral $ T.stringLiteral haskell

literal = liftM Literal (integer <|> float <|> stringLiteral <?> "Literal")

functionCall :: HaskellParser u
functionCall = do
    name <- identifier
    spaces
    arguments <- many (try expression1)
    return $ applyArgs (Var name) arguments
    where
        
infixOp = do
    op <- lexeme $ many1 (oneOf "+-*/")
    return $ "(" ++ op ++ ")"

binopExpr :: HaskellParser u
binopExpr = do
    lhs <- expression
    op <- infixOp
    rhs <- expression <?> fail "Expected expression as second argument to " ++ show op
    return $ applyArgs (Var op) [lhs, rhs]

parens = T.parens haskell

expression1 =  parens expression <|> literal <|> (identifier >>= \n -> return $ Var n)

expression :: HaskellParser u
expression = parens expr <|> expr
    where
        expr = lexeme (ifExpr <|> caseExpr <|> literal <|> functionCall)

reserved = T.reserved haskell
whiteSpace = T.whiteSpace haskell

ifExpr :: HaskellParser u
ifExpr = do
    reserved "if"
    test <- expression
    reserved "then"
    whenTrue <- expression
    reserved "else"
    whenFalse <- expression
    return $ Case test [(PatternMatch "True" [], whenTrue), (PatternMatch "False" [], whenFalse)]

letExpr = do
    reserved "let"
    xs <- many functionDefinition
    reserved "in"
    expr <- expression
    return $ Let xs expr

typename = liftM2 (:) upper (many alphaNum)

patternMatch :: ParsecT String u Identity PatternMatch
patternMatch = do
    name <- typename
    space >> whiteSpace
    xs <- many identifier
    return $ PatternMatch name xs


caseExpr :: HaskellParser u
caseExpr = do
    reserved "case"
    onExpr <- expression
    reserved "of"
    cases <- sepBy aCase (whiteSpace >> newline >> whiteSpace)
    return $ Case onExpr cases
    where
        aCase = do
            pat <- patternMatch
            reservedOp "->"
            expr <- expression
            return (pat, expr)


reservedOp = T.reservedOp haskell

unaryOp :: String -> Expr -> Expr
unaryOp op = Apply (Var op)

binaryOp :: String -> Expr -> Expr -> Expr
binaryOp op = \l r -> applyArgs (Var op) [l,r]

arithmeticOperators = [[Prefix (reservedOp "-"   >> return (unaryOp "negate"))]
                      , [Infix  (reservedOp "*"   >> return (binaryOp "*")) AssocLeft]
                      , [Infix  (reservedOp "/"   >> return (binaryOp "/")) AssocLeft]
                      , [Infix  (reservedOp "+"   >> return (binaryOp "+")) AssocLeft]
                      , [Infix  (reservedOp "-"   >> return (binaryOp "-")) AssocLeft]]
 
arithmeticExpr = buildExpressionParser arithmeticOperators expression



dataDefinition = do
    reserved "data"
    nameOfType <- typename
    reservedOp "="
    ctors <- sepBy1 constructor (reservedOp "|")
    return $ DataDefinition nameOfType ctors
    where
        constructor = do
            name <- typename
            fields <- many typename
            return $ Constructor name fields


functionDefinition = do
    name <- identifier <?> fail "Expected function name"
    arguments <- many (identifier <?> fail "Unexpected token in argument list")
    reservedOp "="
    expr <- expression
    return $ FunctionDefinition name arguments expr

addFunction :: Monad m => FunctionDefinition -> ParsecT s Module m ()
addFunction func@(FunctionDefinition name _ _) = do
    modifyState $ \mod -> mod { functions = Map.insert name func (functions mod) }

addData :: Monad m => DataDefinition -> ParsecT s Module m ()
addData dat@(DataDefinition name _) = do
    modifyState $ \mod -> mod { datas = Map.insert name dat (datas mod) }


file :: FileParser Module
file = do
    definition
    --many definition
    getState
    where
        definition = do
            x <- (functionDefinition >>= return . Left) <|> (dataDefinition >>= return . Right)
            case x of
                Left func -> addFunction func
                Right dat -> addData dat

parseExpr str = parse expression "" str

parseFile :: String -> IO (Either ParseError Module) 
parseFile filename = do
    withFile filename ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStrLn contents
        return $ runParser file (Module Map.empty Map.empty) filename contents

