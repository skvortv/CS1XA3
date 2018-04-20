{-|
Module : ExprType
Description : Contains a type class for 
              differentiable expressions and matrices

Copyright    : (c) Seva Skvortsov @2018
License      : WTFPL
Maintainer   : seva.sk@gmail.com
Stability    : experimental
Portability  : POSIX -}


module ExprType where 
      
-- | This contains a type class for differentiable expressions and also type class for matrices.
data Expr a = Add (Expr a) (Expr a) -- ^ This constructor represents the addition of two expressions of type 'Expr a' 
             | Mult (Expr a) (Expr a) -- ^ This constructor represents the Multiplication of two expressions of type 'Expr a' 
             | Sub (Expr a) (Expr a) -- ^ This constructor represents the Subtration of two expressions of type 'Expr a' 
             | Div (Expr a) (Expr a) -- ^ This constructor represents the Divition of two expressions of type 'Expr a' 
             | Const a -- ^ This constructor represents a Constant
             | Var String -- ^ This constructor represents a Variable where the Variable is a String
             | Cos (Expr a) -- ^ This constructor represents the cosine of an expression of type 'Expr a'
             | Sin (Expr a) -- ^ This constructor represents the sin of an expression of type 'Expr a'
             | Log (Expr a) -- ^ This constructor represents the sin of an expression of type 'Expr a'
             | Exp (Expr a) (Expr a) -- ^ This constructor represents the exponent of two expressions of type 'Expr a'. First expression to the power of second expression.
             | NatExp (Expr a) -- ^ This constructor represents the natural exponent (e^(Expr a) ) of an expression of type 'Expr a'
             | Matrix [[a]] {- ^ This constructor represents a matrix with a List of Lists. ONLY A MATRIX WITH REAL NUMBERS NOT 'Expr' type. Where each List cotains the elements of that row. e.g Matrix [[1,2],[3,4]] 
                                  is the representaion of the matrix 1 2
                                                                     3 4 -}
    deriving (Eq)

-- | getVars is a function that takes an expression and sees which variables (Var a) are present and returns a list of all the variables
getVars :: Expr a -> [String]
getVars (Add e1 e2) = getVars e1 ++ getVars e2
getVars (Mult e1 e2) = getVars e1 ++ getVars e2
getVars (Sub e1 e2) = getVars e1 ++ getVars e2
getVars (Div e1 e2) = getVars e1 ++ getVars e2
getVars (Cos e1) = getVars e1
getVars (Sin e1) = getVars e1
getVars (Log e1) = getVars e1
getVars (Exp e1 e2) = getVars e1 ++ getVars e2
getVars (NatExp e1) = getVars e1
getVars (Matrix _) = []
getVars (Const _) = []
getVars (Var x) = [x]


