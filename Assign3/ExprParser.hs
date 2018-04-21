{-|
Module : ExprParser
Description : This module is used to parse certain strings in Expr types. Details on the syntax is written below. 
              The parser will treat all numbers as doubles. Using parseExprDouble on a string with proper syntax will
              convert the string into the corresponding 'Expr'. 
Copyright    : (c) Seva Skvortsov @2018
License      : WTFPL
Maintainer   : seva.sk@gmail.com
Stability    : experimental
Portability  : POSIX -}


module ExprParser (parseExprDouble) where 

import Text.Parsec
import Text.Parsec.String

import ExprType

{-| parses an expression into Expr Double. The syntax for parser are as follows. + to add, - to subtract, / to divide, * to multiply, 
            ^ for exponentiation, Cos for cosine, Sin for sin, Log for natural logarithm, e^ for natural exponential, any combination of alphabetical characters
            for a variable (expect those previously listed such as Sin), any real numbers for constants, '[]' to indicate a matrix,
            ',' to seperate values in a row of a matrix, ';' to seperate rows of a matrix, () to specify order of operations.  
             Some Examples of valid inputs :
            "5+2^(2-5)" 
            "5.223/(x*2)"
            "e^23"
            "log(dsafa/2)"
            "cos(sin(5.0-x))"
            "5*[2,3,4;1,2,3;2,3,4]"
               -}  
parseExprDouble :: String -- ^ The string to be parsed
                 -> Expr Double -- ^ The resulting parsed expression

parseExprDouble ss = case parse exprD "" ss of 
                     Left err -> error $ "Parse Error: " ++ show err
                     Right val -> val 
-- | exprD is the expression double parser
exprD :: Parser (Expr Double)
exprD = terms `chainl1` addop 
terms = single `chainl1` mulop
single = single1 `chainl1` expop
single1 = (cosop exprD) <|> single2
single2 = (sinop exprD) <|> single3
single3 = (logop exprD) <|> single4
single4 = (natExpop exprD) <|> factors
factors = factors1 <|> matrix
factors1 = variables <|> constants
variables = parens exprD <|> var

-- | parser that parses addition and subtraction operations. Parses + and -
addop :: Parser (Expr Double -> Expr Double -> Expr Double)
addop = do { symbol "+"; 
            return (Add)} <|> do { symbol "-"; return (Sub) }
-- | parser that parses multiplication and divition operations. Parses * and - 
mulop :: Parser (Expr Double -> Expr Double -> Expr Double)
mulop = do { symbol "*"; return (Mult)} <|> do{ symbol "/"; return (Div) }
-- | parser that parses the cos operation. Parses cos
cosop :: Parser (Expr Double) -> Parser (Expr Double)
cosop p =string "Cos" >> spaces >>  p >>= (\expr -> return $ Cos expr )
-- | parser that parses the sin operation. Parses sin
sinop :: Parser (Expr Double) -> Parser (Expr Double)
sinop p = string "Sin" >> spaces >>  p >>= (\expr -> return $ Sin expr)
-- | parser that parses the natural log operation. Parses log 
logop :: Parser (Expr Double) -> Parser (Expr Double)
logop p = string "Log">> spaces >>  p >>= (\expr -> return $ Log expr)
-- | parser that parses the natural exponential operation. Parses e^
natExpop :: Parser (Expr Double) -> Parser (Expr Double)
natExpop p = string "e^">> spaces >>  p >>= (\expr -> return $ NatExp expr)
-- | parser that parses the exponential operation. Parses ^
expop :: Parser (Expr Double -> Expr Double -> Expr Double)
expop = do {symbol "^"; return (Exp)}
-- | parser that parses a matix. Parses "[]", "," ";" ;
matrix :: Parser (Expr Double)
matrix = do {spaces; 
             ds <- (matrixparse);
             spaces;
          return (Matrix ds) } 
-- | parser that parses constants. Parses any real number
constants :: Parser (Expr Double)
constants = do {spaces; 
                ds <- (double);
                spaces;
          return (Const ds) } 
-- | parser that parses variables. Parses letters
var :: Parser (Expr Double)
var = do {spaces;
          cs <- many1 (letter);
          spaces;
          return (Var cs) }

-- * Utility Combinators

double :: Parser Double
double = (fmap read $ doubleDigits)

doubleDigits :: Parser String
doubleDigits = do {ds <- try negDigits <|> digits;
                   rs <- try decimalDigits <|> return "";
                   return $ ds ++ rs }
decimalDigits :: Parser String
decimalDigits = do { d <- char '.' ;
                     rm <- digits ;
                     return $ d:rm }
parens :: Parser a -> Parser a
parens p = do { spaces;
                char '(';
                spaces;
                cs <- p;
                spaces;
                char ')';
                return cs }

symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }

identifier :: Parser String
identifier = do { cs <- many1 (alphaNum) ;
                return cs } 

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits


row :: Parser [Double]
row = do {  xs <- sepBy double (symbol ",") ;
             return xs}

rows :: Parser [Double] -> Parser [[Double]]
rows p = do {  xs <- sepBy p (symbol ";") ;
             return xs}

squarebrackets :: Parser a -> Parser a
squarebrackets p = do { spaces;
                char '[';
                spaces;
                cs <- p;
                spaces;
                char ']';
                return cs }

matrixparse :: Parser [[Double]]
matrixparse = squarebrackets (rows row)