{-# LANGUAGE FlexibleContexts #-}
module Expression (
    Module(..),
    Closure(..),
    Literal(..),
    Identifier(..),
    Expr(..),
    Pattern(..),
    FunctionDefinition(..),

    applyArgs,
    emptyClosure,
    lookupExpr,
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

data Identifier = Identifier String deriving(Eq, Show)

type Typename = String
data Pattern = Pattern Typename [Pattern]
             | PatternLiteral Literal
             | Binding Identifier
             deriving(Eq, Show)

data Literal = Integer Integer
             | Float Double
             | StringLiteral String
             deriving (Eq, Show)

data Expr = Literal Literal
          | Var String
          | Apply Expr Expr
          | Lambda String Expr
          | Case Expr [(Pattern, Expr)]
          | Let [(String, Expr)] Expr
          deriving (Eq, Show)

data FunctionDefinition = FunctionDefinition String Expr deriving(Eq, Show)

data Constructor = Constructor String [Constructor]
                 | Bind String
                 deriving(Eq, Show)
data DataDefinition = DataDefinition String [Constructor] deriving(Eq, Show)


data Module = Module {
    _variables :: Map.Map String Expr,
    datas :: Map.Map String DataDefinition
    } deriving(Eq, Show)

data Closure = Closure {
    variables :: Map.Map String Expr,
    outer :: Closure
    }
    | Top Module
    deriving(Eq)

emptyClosure = Top (Module Map.empty Map.empty)

lookupExpr name (Top (Module vars _)) = Map.lookup name vars
lookupExpr name (Closure vars outer) = case Map.lookup name vars of
                                            Just expr -> Just expr
                                            Nothing -> lookupExpr name outer

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
    return $ Case test [(Pattern "True" [], whenTrue), (Pattern "False" [], whenFalse)]

letExpr = do
    reserved "let"
    xs <- many functionDefinition
    reserved "in"
    expr <- expression
    let ys = map (\(FunctionDefinition x y) -> (x,y)) xs
    return $ Let ys expr

typename = liftM2 (:) upper (many alphaNum)

patternMatch :: ParsecT String u Identity Pattern
patternMatch = do
    name <- typename
    space >> whiteSpace
    xs <- many patternMatch
    return $ Pattern name xs


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
            return $ Constructor name (map Bind fields)


toLambda argNames expr = foldr Lambda expr argNames

functionDefinition = do
    name <- identifier <?> fail "Expected function name"
    arguments <- many (identifier <?> fail "Unexpected token in argument list")
    reservedOp "="
    expr <- expression
    return $ FunctionDefinition name expr

addFunction :: Monad m => FunctionDefinition -> ParsecT s Module m ()
addFunction (FunctionDefinition name expr) = do
    modifyState $ \mod -> mod { _variables = Map.insert name expr (_variables mod) }

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

parseExpr str = parse arithmeticExpr "" str

parseFile :: String -> IO (Either ParseError Module) 
parseFile filename = do
    withFile filename ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStrLn contents
        return $ runParser file (Module Map.empty Map.empty) filename contents

