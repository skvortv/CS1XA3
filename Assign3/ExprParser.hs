module ExprParser (parseExprDouble, parseExprFloat) where 

import Text.Parsec
import Text.Parsec.String

import ExprType
-- | parses an expression into Expr Doubl
-- takes a string with + - * / ^ e^  Cos , Sin, Log  and variables (only letters) and returns my expression type representation of that string no padding at beginning 
parseExprDouble :: String -- ^ The string to be parsed
                 -> Expr Double -- ^ The resulting parsed expression

parseExprDouble ss = case parse exprD "" ss of 
                     Left err -> error $ "Parse Error: " ++ show err
                     Right val -> val 

parseExprFloat :: String -> Expr Float
parseExprFloat ss = case parse exprF "" ss of 
                     Left err -> error $ "Parse Error: " ++ show err
                     Right val -> val 


exprD :: Parser (Expr Double)
exprD = terms `chainl1` addop 
terms = single `chainl1` mulop
single = single1 `chainl1` expop
single1 = (cosop exprD) <|> single2
single2 = (sinop exprD) <|> single3
single3 = (logop exprD) <|> single4
single4 = (natExpop exprD) <|> factors
factors = variables <|> constants
variables = parens exprD <|> var


addop :: Parser (Expr Double -> Expr Double -> Expr Double)
addop = do { symbol "+"; 
            return (Add)} <|> do { symbol "-"; return (Sub) }

mulop :: Parser (Expr Double -> Expr Double -> Expr Double)
mulop = do { symbol "*"; return (Mult)} <|> do{ symbol "/"; return (Div) }

cosop :: Parser (Expr Double) -> Parser (Expr Double)
cosop p =string "Cos" >> spaces >>  p >>= (\expr -> return $ Cos expr )

sinop :: Parser (Expr Double) -> Parser (Expr Double)
sinop p = string "Sin" >> spaces >>  p >>= (\expr -> return $ Sin expr)

logop :: Parser (Expr Double) -> Parser (Expr Double)
logop p = string "Log">> spaces >>  p >>= (\expr -> return $ Log expr)

natExpop :: Parser (Expr Double) -> Parser (Expr Double)
natExpop p = string "e^">> spaces >>  p >>= (\expr -> return $ NatExp expr)

expop :: Parser (Expr Double -> Expr Double -> Expr Double)
expop = do {symbol "^"; return (Exp)}



constants :: Parser (Expr Double)
constants = do {spaces; 
                ds <- (double);
                spaces;
          return (Const ds) } 

var :: Parser (Expr Double)
var = do {spaces;
          cs <- many1 (letter);
          spaces;
          return (Var cs) }


exprF :: Parser (Expr Float)
exprF = error "Do this"

{- ------------------------------------------------------------------------------------------------------------
 - Utility Combinators
 - ------------------------------------------------------------------------------------------------------------
 -}


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
parens p = do { char '(';
                cs <- p;
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

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits