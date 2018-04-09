{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-| 
Module : ExprDiff
Description : Contains a type calss and instance for
              differentiable expressions
Copyright    : (c) Seva Skvortsov @2018
License      : WTFPL
Maintainer   : seva.sk@gmail.com
Stability    : experimental
Portability  : POSIX -}

module ExprDiff where 

import Data.Map as Map
import ExprType
import ExprPretty

{-Class DiffExpr
-Class of Differentiable Expressions
- -------------------------------------
-Methods
-   eval : Given a dictionary of variable indentifiers 
-          and an Expr, evaluate that Expr to a value
-   simplify : Given a possibly incomplete dictionarty of variable
-              indetifiers and an Expr, evaluate Expr 
-              as much as possible and reduce 
-   partDiff : takes variable indentifier and an Expr 
-              and differentiate in term of the
-              identifier
-Default Methods
-   !+,~8.val,var : correspond to optimize type
-                   wrappers
-Example DSL use:
-   (var "x") !+ (val 0.0 !* var "y")
-   should become : (var "x")
-}

-- | This class operates over the 'Expr' Data type
class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a

  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!-) :: Expr a -> Expr a -> Expr a
  e1 !- e2 = simplify (Map.fromList []) $ Sub e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  (!/) :: Expr a -> Expr a -> Expr a
  e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2
  val :: a -> Expr a
  val x = Const x
  var :: String -> Expr a
  var x = Var x


instance (Num a, Eq a,Fractional a, Floating a) => DiffExpr a where
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Sub e1 e2) = eval vrs e1 - eval vrs e2
  eval vrs (Div e1 e2) = case (eval vrs e2) of 
                          0 -> error "divide by zero"
                          x -> eval vrs e1 / eval vrs e2
  eval vrs (Exp e1 e2) = eval vrs e1 ** eval vrs e2 
  eval vrs (Cos x) =  cos (eval vrs x) 
  eval vrs (Sin x) = sin (eval vrs x)
  eval vrs (NatExp x) = exp (eval vrs x)
  eval vrs (Log x) = log (eval vrs x)
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  simplify _ (Add (Mult (Const a)  (Var y)) (Mult (Const b)  (Var x)))   = if x == y then 
                                       Mult (Const (a+b) ) (Var y)
                                     else
                                       (Add (Mult (Const a)  (Var y)) (Mult (Const b)  (Var x)))
  simplify _ (Add (Mult (Const a)  (Var y)) (Var x) )   = if x == y then 
                                       Mult (Const (a+1) ) (Var y)
                                     else
                                       (Add (Mult (Const a)  (Var y)) (Var x) )  
  simplify _ (Add (Var y) (Mult (Const b)  (Var x)))   = if x == y then 
                                       Mult (Const (b+1) ) (Var y)
                                     else
                                       (Add (Var y) (Mult (Const b)  (Var x)))
  simplify _ (Add (Const a) (Const b)) = Const (a+b)
  simplify _ (Mult (Const a) (Const b)) = Const (a*b)
  simplify _ (Mult (Var x) (Const 0) ) = (Const 0)
  simplify _ (Mult (Const 0) (Var x) ) = (Const 0)  
  simplify _ e = e -- #TODO finish me!
  partDiff _ e = e -- #TODO finish me!

