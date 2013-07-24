{-# LANGUAGE FlexibleContexts #-}
module Expression where
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

data Expr = IntegerValue Integer
          | FloatValue Double
          | StringLiteral String
          | Call String [Expr]
          | If Expr Expr Expr
          | Case Expr [(PatternMatch, Expr)]
          deriving (Eq, Show)

data FunctionDefinition = FunctionDefinition String [String] Expr

data Constructor = Constructor String [String]
data DataDefinition = DataDefinition String [Constructor]

data Module = Module {
    functions :: Map.Map String FunctionDefinition,
    datas :: Map.Map String DataDefinition
    }

type HaskellParser u = ParsecT String u Identity Expr
type FileParser a = ParsecT String Module Identity a
--type HaskellParser u = T.TokenParser ()

identifier = T.identifier haskell

integer :: HaskellParser u
integer = liftM IntegerValue $ T.integer haskell

float = liftM FloatValue $ T.float haskell

stringLiteral = liftM StringLiteral $ T.stringLiteral haskell

literal = integer <|> float <|> stringLiteral

functionCall :: HaskellParser u
functionCall = do
    name <- identifier
    spaces
    arguments <- many (spaces >> expression)
    return $ Call name arguments

infixOp = do
    op <- many1 (oneOf "+-*/")
    return $ "(" ++ op ++ ")"

binopExpr :: HaskellParser u
binopExpr = do
    lhs <- expression
    op <- infixOp
    rhs <- expression
    return $ Call op [lhs, rhs]

parens = T.parens haskell


expression :: HaskellParser u
expression = do
    parens expr <|> expr
    where
        expr = spaces *> (ifExpr <|> caseExpr <|> functionCall <|> literal) <* spaces

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
    return $ If test whenTrue whenFalse

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
unaryOp op = \x -> Call op [x]

binaryOp :: String -> Expr -> Expr -> Expr
binaryOp op = \l r -> Call op [l,r]

arithmeticOperators = [[Prefix (reservedOp "-"   >> return (unaryOp "negate"))]
                      , [Infix  (reservedOp "*"   >> return (binaryOp "*")) AssocLeft]
                      , [Infix  (reservedOp "/"   >> return (binaryOp "/")) AssocLeft]
                      , [Infix  (reservedOp "+"   >> return (binaryOp "+")) AssocLeft]
                      , [Infix  (reservedOp "-"   >> return (binaryOp "-")) AssocLeft]]
 
boolOperators = [ [Prefix (reservedOp "not" >> return (unaryOp "not"))]
                , [Infix  (reservedOp "and" >> return (binaryOp "and")) AssocLeft]
                , [Infix  (reservedOp "or"  >> return (binaryOp "or"))  AssocLeft]]


arithmeticExpr = buildExpressionParser arithmeticOperators expression
boolExpr = buildExpressionParser boolOperators expression



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


identifierDefinition = do
    name <- identifier
    arguments <- many identifier
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
    many defintion
    getState
    where
        defintion = do
            x <- (identifierDefinition >>= return . Left) <|> (dataDefinition >>= return . Right)
            case x of
                Left func -> addFunction func
                Right dat -> addData dat

parseExpr str = parse expression "" str

parseFile :: String -> IO (Either ParseError Module) 
parseFile filename = do
    withFile filename ReadMode $ \handle -> do
        contents <- hGetContents handle
        return $ runParser file (Module Map.empty Map.empty) filename contents

