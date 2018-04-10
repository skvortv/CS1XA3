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
  (!^) :: Expr a -> Expr a -> Expr a
  e1 !^ e2 = simplify (Map.fromList []) $ Exp e1 e2
  matrix :: [[a]] -> Expr a
  matrix x = Matrix x
  val :: a -> Expr a
  val x = Const x
  var :: String -> Expr a
  var x = Var x


instance (Num a, Eq a, Fractional a, Floating a) => DiffExpr a where
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
  simplify _ (Mult (Matrix x) (Matrix y) ) = Matrix (matrixMult x y)
  simplify _ (Add (Matrix x ) (Matrix y)) = Matrix (addmatrix x y)
  simplify _ (Mult (Const x)  (Matrix y) ) = Matrix (multMatrixByC x y)
  simplify _ (Mult (Matrix y) (Const x)) = Matrix (multMatrixByC x y)
  simplify _ e = e -- #TODO finish me!
  partDiff _ e = e -- #TODO finish me!


--helper functions
lmao :: (Num a) => [a] -> [a] -> [(a,a)]
lmao [] ys = []
lmao xs [] = []
lmao (x:xs) (y:ys) = (x,y):(lmao xs ys)

lmao2 :: (Num a) => [(a,a)] -> [a]
lmao2 xs = [ tupleadd x | x <- xs ] 

addmatrix ::(Num a) => [[a]] -> [[a]] -> [[a]]
addmatrix [] ys = []
addmatrix xs [] = []
addmatrix (x:xs) (y:ys) = if (length x) == (length y)
                          then 
                            ((lmao2 (lmao x y)) : (addmatrix (xs) (ys)))
                          else
                             error "Matrix dimenstions are not the same"
multMatrixByC :: (Num a) => a -> [[a]] -> [[a]]
multMatrixByC c [] = []
multMatrixByC c (x:xs) = (Prelude.map (*c) x): (multMatrixByC c xs)

tupleadd :: (Num a) => (a,a) -> a
tupleadd (x,y) = x+y  

matrixMult ::(Num a) => [[a]] -> [[a]] -> [[a]]
matrixMult [] ys = []
matrixMult xs [] = []
matrixMult (x:xs) ys = (matstep2 x ys): (matrixMult xs ys) 

matstep2 :: (Num a) => [a] -> [[a]] -> [a]
matstep2 xs [] = []
matstep2 xs ys = (matstep1 xs [head i | i <- ys]) : (matstep2 xs [tail a | a <- ys, (length a) > 1 ])

matstep1 ::(Num a) => [a] -> [a] -> a
matstep1 [] [] = 0
matstep1 xs [] = error "Matrices not right size for multiplication"
matstep1 [] ys = error "Matrices not right size for multiplication"
matstep1 (x:xs) (y:ys) = (x*y) + (matstep1 xs ys)    


--[[1,2,3],[1,2,3]] [[1,2], [1,2],[1,2]]
