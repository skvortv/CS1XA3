{-|
Module : ExprPretty
Description :  Contains the Show instance of the 'Expr' Data type. Makes the type much easier to read. Add shows !+ , Mult shows !*, Var x shows var x,
               Const x shows val x, Cos shows cos, Sin shows sin, NatExp shows e^, Log shows Log, Exp show !^, Sub shows !-, Div shows !/, and Matrix
               prints each row on a seperate line to form a matrix.
Copyright    : (c) Seva Skvortsov @2018
License      : WTFPL
Maintainer   : seva.sk@gmail.com
Stability    : experimental
Portability  : POSIX -}


module ExprPretty where 

import ExprType

-- | Show instance of the 'Expr a'. Makes values much easier to read
instance (Show a) => Show (Expr a) where
    show (Add e1 e2) = parens (show e1) ++ " !+ " ++ parens (show e2)
    show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
    show (Var x) = parens $ "var " ++ "\"" ++ x ++ "\""
    show (Const x) = "val " ++ show x
    show (Cos x)  = "Cos" ++ parens (show x)
    show (Sin x) = "Sin" ++ parens (show x)
    show (NatExp x) = "e^" ++ parens (show x)
    show (Log x) = "Log" ++ parens (show x)
    show (Exp e1 e2) = parens (show e1) ++ " !^ " ++ parens (show e2)
    show (Sub e1 e2) = parens (show e1) ++ " !- " ++ parens (show e2)
    show (Div e1 e2) = parens (show e1) ++ " !/ " ++ parens (show e2)
    show (Matrix (x:xs)) =  "\n" ++(box (show x)) ++ (show (Matrix (xs)))
    show (Matrix []) =  "" 
  
-- * Miscellaneous Functions
-- | Funtion that takes a String and puts parentheses around that string
parens :: String -> String
parens ss = "(" ++ ss ++ ")"
box :: String -> String
-- | Function that takes a String and puts | | around that string. Used to show Matrices prettier
box ss = "|" ++ ss ++ "|"
