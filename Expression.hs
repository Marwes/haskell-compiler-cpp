{-# LANGUAGE FlexibleContexts #-}
module Expression (
    Module(..),
    Closure(..),
    Literal(..),
    Identifier(..),
    Expr(..),
    Pattern(..),
    FunctionDefinition(..),
    Decl(..),

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


data Decl = DataDecl DataDefinition
          | FunctionDecl FunctionDefinition
          deriving (Eq, Show)

declIdentifier (DataDecl (DataDefinition name _)) = name
declIdentifier (FunctionDecl (FunctionDefinition name _)) = name

data Module = Module {
    _variables :: Map.Map String Expr,
    declarations :: Map.Map String Decl
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

showLiteral (Integer i) = show i
showLiteral (Float f) = show f
showLiteral (StringLiteral s) = s

showExpr (Literal l) = showLiteral l
showExpr (Var x) = ' ':x
showExpr (Apply func expr) = "(" ++ showExpr func ++ " " ++ showExpr expr ++ ")"
showExpr (Lambda bind expr) = "\\" ++ bind ++ " -> " ++ showExpr expr
showExpr (Case expr patterns) = "case " ++ showExpr expr ++ " of\n" ++ choices
    where
        choices = unlines $ map showCase patterns
        showCase (pattern, expr) = showPattern pattern ++ " -> " ++ showExpr expr
        showPattern (Pattern typename patterns) = "(" ++ typename ++ " " ++ concat (map showPattern patterns) ++ ")"
        showPattern (PatternLiteral literal) = showLiteral literal
        showPattern (Binding (Identifier s)) = s

applyArgs func args = foldl Apply func args

lambdas :: [String] -> Expr -> Expr
lambdas names expr = foldr Lambda expr names

lexeme = T.lexeme haskell

identifier = T.identifier haskell <?> "Identifier"

natural = liftM Integer (T.natural haskell) <?> "Integerliteral"

float = liftM Float (T.float haskell) <?> "Floatliteral"

stringLiteral = liftM StringLiteral $ T.stringLiteral haskell

literal = liftM Literal (natural <|> float <|> stringLiteral <?> "Literal")

parens = T.parens haskell

operator = T.operator haskell

tuple :: Parser u Expr
tuple = do
    expressions <- parens (sepBy1 expression (char ','))
    let ctor = take (length expressions - 1) (repeat ',')
    return $ applyArgs (Var ctor) expressions

prefixOperator :: Parser u Expr
prefixOperator = operator >>= return . Var

expression1 :: Parser u Expr
expression1 = do
    expr <- exprParser
    args <- optionMaybe exprParser
    case args of
        Nothing -> return expr
        Just arg -> return $ Apply expr arg
    where
        exprParser = try tuple <|> parens (expression <|> prefixOperator) <|> literal <|> (identifier >>= \n -> return $ Var n)


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
 
expression = buildExpressionParser arithmeticOperators expression1



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



functionDefinition = do
    name <- identifier <?> fail "Expected function name"
    arguments <- many (identifier <?> fail "Unexpected token in argument list")
    reservedOp "="
    expr <- expression
    return $ FunctionDefinition name (lambdas arguments expr)

file :: FileParser [Decl]
file = whiteSpace *> many decl <* whiteSpace
    where
        decl = do
            x <- (functionDefinition >>= return . FunctionDecl) <|> (dataDefinition >>= return . DataDecl)
            return x

parseExpr str = parse expression "" str

parseFile :: String -> IO (Either ParseError Module) 
parseFile filename = do
    withFile filename ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStrLn contents
        let m = liftM declMap $ runParser file (Module Map.empty Map.empty) filename contents
            declMap x = Module Map.empty $ Map.fromList $ map (\decl -> (declIdentifier decl, decl)) x
        return m

