module ExprType where 

{- Datatype for Numerical Expressions 
- ------------------------------------
-Suportes the follwing operations 
-Add - binary addition
-Mult - binary multipicatoins
-Const - wraps a constant value
-Var - wraps a variable indentifier -}
-- | a datatype for encoding numeric expressions
data Expr a = Add (Expr a) (Expr a) 
             | Mult (Expr a) (Expr a)
             |Sub (Expr a) (Expr a) 
             | Div (Expr a) (Expr a) 
             | Const a
             | Var String
             | Cos (Expr a)
             | Sin (Expr a)
             | Log (Expr a)
             | Exp (Expr a) (Expr a)
             |NatExp (Expr a)
    deriving Eq
{- getVars
-----------------------------------
-Given an expression, retrives a list of all
-variable indentifiers

-}

getVars :: Expr a -> [String]
getVars (Add e1 e2) = getVars e1 ++ getVars e2
getVars (Mult e1 e2) = getVars e1 ++ getVars e2
getVars (Const _) = []
getVars (Var x) = [x]


