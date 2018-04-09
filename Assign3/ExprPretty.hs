module ExprPretty where 

import ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

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