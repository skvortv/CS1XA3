{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module : ExprDiff
Description :  Contains a typeclass definitions for methods for the Expr type
Copyright    : (c) Seva Skvortsov @2018
License      : WTFPL
Maintainer   : seva.sk@gmail.com
Stability    : experimental
Portability  : POSIX -}

module ExprDiff where 

import Data.Map as Map
import ExprType
import ExprPretty


-- | This class operates over the 'Expr' Data type, contains definitions for method for the 'Expr'
class DiffExpr a where
  -- | Given a dictionary of variable indentifiers and an Expr, evaluate that Expr to a single value
  eval :: Map.Map String a -- ^ Dictionary of indentifiers
          -> Expr a -- ^ Expression to be evaluated
          -> a -- ^ resulting value

  -- | Given a posibly incomplete dictionary of varuable indetifiers and Expr, evluate the Expr as much as possible and reduce
  simplify :: Map.Map String a -- ^ Dictionary of indentifiers
              -> Expr a -- ^ Expression to be simplifyed
              -> Expr a -- ^ resulting Expression

  -- | Takes a string of a sngle variable indentifier and an Expr and differentiats that Expr in terms of the indentifier
  partDiff :: String -- ^ String with the indentifier
              -> Expr a -- ^ Expression to be partially differentiated
              -> Expr a -- ^ Resulting Expression
 
  -- |  corresponds to the Add type wrapper to optimize Add type. tries to simplify the Expr 
  (!+) :: Expr a -- ^ First Expression 
          -> Expr a -- ^ Second Expression
          -> Expr a -- ^ Resulting simplified  Add Expression 
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2

  -- | corresponds to the Sub type wrappers to optimize Sub type. tries to simplify the Expr 
  (!-) :: Expr a -- ^ First Expression given
          -> Expr a -- ^ Second Expression given
          -> Expr a -- ^ Resulting simplified Sub Expression
  e1 !- e2 = simplify (Map.fromList []) $ Sub e1 e2

  -- | corresponds to the Mult type wrappers to optimize Mult type. tries to simplify the Expr 
  (!*) :: Expr a -- ^ First Expression given
          -> Expr a -- ^ Second Expression given
          -> Expr a -- ^ Resulting simplified Mult Expression
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2

  -- | corresponds to the Div type wrappers to optimize Div type. tries to simplify the Expr
  (!/) :: Expr a -- ^ First Expression given
        -> Expr a -- ^ Second Expression given
        -> Expr a -- ^ Resulting simplified Div Expression
  e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2
  -- | corresponds to the Exp type wrappers to optimize Exp type. tries to simplify the Expr
  (!^) :: Expr a -- ^ First Expression given
          -> Expr a -- ^ Second Expression given
          -> Expr a -- ^ Resulting simplified Exp Expression
  e1 !^ e2 = simplify (Map.fromList []) $ Exp e1 e2
  -- | corresponds to the Cos type wrappers to optimize Cos type. tries to simplify the Expr
  mycos :: Expr a -- ^ Expression given
          -> Expr a -- ^ Resulting simplified Cos Expression
  mycos e1 = simplify (Map.fromList []) $ Cos e1 
  -- | corresponds to the Sin type wrappers to optimize Sin type. tries to simplify the Expr
  mysin :: Expr a -- ^ Expression given
          -> Expr a -- ^ Resulting simplified Sin Expression
  mysin e1 = simplify (Map.fromList []) $ Sin e1
  -- | corresponds to the Log type wrappers to optimize Log type. tries to simplify the Expr
  mylog :: Expr a -- ^ Expression given
          -> Expr a -- ^ Resulting simplified Log Expression
  mylog e1 = simplify (Map.fromList []) $ Log e1
  -- | corresponds to the NatExp type wrappers to optimize Natexp type. tries to simplify the Expr
  natexp :: Expr a -- ^ Expression given
            -> Expr a -- ^ Resulting simplified NatExp Expression
  natexp e1 = simplify (Map.fromList []) $ NatExp e1   
  -- | Takes a List of Lists and wraps it in a Matrix constructor 
  matrix :: [[a]] -- ^ Given matrix represented by a list of lists 
            -> Expr a -- ^ Resulting Matrix Expression
  matrix x = Matrix x
  -- | Takes a value of type a and wraps it in Const constructor 
  val :: a -- ^ given value
        -> Expr a -- ^ Resulting Expression  
  val x = Const x
  -- | Takes a string and wraps it in a Var constructor
  var :: String -- ^ Giving String 
        -> Expr a -- ^ Resulting Expression
  var x = Var x

-- | Is an instance for Floating class number for the class definition DiffExpr. It contains the fuction definitions for eval, partDiff, simplify. Eval does not work on matrices. partDiff does not work for Exp or Matrices
instance (Eq a, Floating a) => DiffExpr a where
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

  simplify vrs (Const x) = Const x
  simplify vrs (Matrix x) = Matrix x
  simplify vrs (Mult (Const 0) e1 ) = Const 0
  simplify vrs (Mult e1 (Const 0) ) = Const 0
  simplify vrs (Mult e1 (Const 1)) = simplify vrs e1
  simplify vrs (Mult (Const 1) e1) =simplify vrs e1
  simplify vrs (Add e1 (Const 0)) = simplify vrs e1
  simplify vrs (Add (Const 0) e1) = simplify vrs e1
  simplify vrs (Exp (e1) (Const 0)) = Const 1
  simplify vrs (Exp (Const 0) e1) = Const 0

  simplify vrs (Var x) = case Map.lookup x vrs of 
                          Just v -> Const v
                          Nothing -> Var x

  simplify vrs (Add e1 (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Add (Const v) (simplify vrs e1))
                                    Nothing -> Add (simplify vrs e1) (Var x)

  simplify vrs (Add (Var x) e1) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Add (simplify vrs e1) (Const v) )
                                    Nothing -> Add (Var x) (simplify vrs e1)


  simplify vrs (Sub e1 (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Sub (simplify vrs e1) (Const v) )
                                    Nothing -> Sub (simplify vrs e1) (Var x)

  simplify vrs (Sub (Var x) e1) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Sub (Const v)  (simplify vrs e1)  )
                                    Nothing -> Sub (Var x) (simplify vrs e1)

  simplify vrs (Mult (Var x) e1) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Mult (simplify vrs e1) (Const v) )
                                    Nothing -> Mult (Var x) (simplify vrs e1)                

  simplify vrs (Mult e1 (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Mult (Const v) (simplify vrs e1) )
                                    Nothing -> Mult (simplify vrs e1) (Var x)                
  simplify vrs (Div (Var x) e1) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Div (Const v) (simplify vrs e1)  )
                                    Nothing -> Div (Var x) (simplify vrs e1)                

  simplify vrs (Div e1 (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Div (simplify vrs e1) (Const v)  )
                                    Nothing -> Div (simplify vrs e1) (Var x)  

  simplify vrs (Exp (Var x) e1) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Exp (Const v) (simplify vrs e1) )
                                    Nothing -> Exp (Var x) (simplify vrs e1)                

  simplify vrs (Exp e1 (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Exp (simplify vrs e1)  (Const v) )
                                    Nothing -> Exp (simplify vrs e1) (Var x)
  simplify vrs (Cos (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Cos (Const v) )
                                    Nothing -> Cos (Var x) 
  simplify vrs (Sin (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Sin (Const v) )
                                    Nothing -> Sin (Var x)
  simplify vrs (Log (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Log (Const v) )
                                    Nothing -> Log (Var x)
  simplify vrs (NatExp (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (NatExp (Const v) )
                                    Nothing -> NatExp (Var x) 

  simplify vrs (Add e1 e2) = case ((simplify vrs e1),(simplify vrs e2)) of 
                             (Const x , Const y) -> Const (x + y)
                             (Matrix x, Matrix y) -> Matrix (addmatrix x y)
                             _ -> Add (simplify vrs e1)  (simplify vrs e2)

  simplify vrs (Sub e1 e2) = case ((simplify vrs e1) ,(simplify vrs e2)) of 
                             (Const x,Const y)->(Const (x-y) )
                             (Matrix x, Matrix y) -> Matrix (submatrix x y)
                             _ -> Sub (simplify vrs e1)  (simplify vrs e2)

  simplify vrs (Mult e1 e2) = case ((simplify vrs e1) ,(simplify vrs e2)) of 
                             (Const x,Const y)->(Const (x*y) ) 
                             (Const x, Matrix y) -> Matrix (multMatrixByC x y)
                             (Matrix y, Const x) -> Matrix (multMatrixByC x y)
                             (Matrix x, Matrix y) -> Matrix (matrixMult x y)
                             _ -> Mult (simplify vrs e1)  (simplify vrs e2)

  simplify vrs (Div e1 e2) = if e1 == e2 
                             then (Const 1)
                             else 
                              case ((simplify vrs e1) ,(simplify vrs e2)) of 
                                (Const x,Const y)->(Const (x/y) ) 
                                (Const x, Matrix y) -> Matrix (multMatrixByC (1/x) y)
                                (Matrix y, Const x) -> Matrix (multMatrixByC (1/x) y)
                                _ -> Div (simplify vrs e1)  (simplify vrs e2) 

  simplify vrs (Exp e1 e2) = case ((simplify vrs e1) ,(simplify vrs e2)) of 
                             (Const x,Const y)->(Const (x**y) ) 
                             _ -> Exp (simplify vrs e1)  (simplify vrs e2)
  simplify vrs (Cos e1) = case (simplify vrs e1) of 
                             Const x -> Const (cos x)
                             _-> Cos (simplify vrs e1)
  simplify vrs (Sin e1) = case (simplify vrs e1) of 
                             Const x -> Const (sin x)
                             _-> Sin (simplify vrs e1)
  simplify vrs (Log e1) = case (simplify vrs e1) of 
                             Const x -> Const (log x)
                             _-> Log (simplify vrs e1)
  simplify vrs (NatExp e1) = case (simplify vrs e1) of 
                             Const x -> Const (exp x)
                             _-> NatExp (simplify vrs e1)



  partDiff t (Var x) | x == t = Const 1
                     | otherwise = (Const 0)
  partDiff _ (Const _) = Const 0
  partDiff t (Add e1 e2) =(Add (partDiff t e1) (partDiff t e2))
  partDiff t (Mult e1 e2) = (Add (Mult (partDiff t e1) e2) (Mult e1 (partDiff t e2)))
  partDiff t (Sub e1 e2) =(Sub (partDiff t e1) (partDiff t e2))
  partDiff t (Div e1 e2) =(Div (Sub (Mult (partDiff t e1) e2) (Mult e1 (partDiff t e2))) (Exp e2 (Const 2) ))
  partDiff t (Cos x) =(Mult (Const (-1) ) (Mult (partDiff t x) (Sin x)))
  partDiff t (Sin x) = (Mult (partDiff t x) (Cos x))
  partDiff t (Log x) =(Div ( partDiff t x) x)
  partDiff t (NatExp x) = Mult (NatExp x) (partDiff t x)
  partDiff t (Exp (Const x) e1) = Mult (Exp (Const x) e1) (Log (Const x))
  partDiff t (Exp e1 (Const x)) = Mult (Const (x)) (Exp e1 (Const (x-1) ) )
  

-- * Miscellaneous Functions
-- | This fuction takes 2 lists and combines then to form a list of tuples with matching element indexs for each tuple. Need this for addmatrix. e.g listadd [1,1] [2,2] == [(1,2), (1,2)].
listadd :: (Num a) => [a] -> [a] -> [(a,a)]
listadd [] ys = []
listadd xs [] = []
listadd (x:xs) (y:ys) = (x,y):(listadd xs ys)
-- | This fuction adds the elements in the tuples to form a list. e.g listcombine2 [(1,2), (3,4)] == [3,7].
listcombine2 :: (Num a) => [(a,a)] -> [a]
listcombine2 xs = [ tupleadd x | x <- xs ] 

-- | This function is the same as listadd only for subtraction. e.g listsubract [(1,1), (3,6)] == [0,-3].
listsubtract :: (Num a) => [(a,a)] -> [a]
listsubtract xs = [ tuplesub x | x <- xs ] 

-- | This function takes a matrix which is represented by a list of list, with each list being a row, and adds them together to get a martix (List of Lists). e.g addmatrix [[1,2],[3,4] [[1,1],[1,1]] == [[2,3],[4,5]].
addmatrix ::(Num a) => [[a]] -> [[a]] -> [[a]]
addmatrix [] ys = []
addmatrix xs [] = []
addmatrix (x:xs) (y:ys) = if (length x) == (length y)
                          then 
                            ((listcombine2 (listadd x y)) : (addmatrix (xs) (ys)))
                          else
                             error "Matrix dimenstions are not the same"
-- | This function takes a matrix which is represented by a list of list, with each list being a row, and subtracts them to get a matrix (List of Lists). e.g submatrix [[3,4]] [[1,2]] == [[2,2]]
submatrix ::(Num a) => [[a]] -> [[a]] -> [[a]]
submatrix [] ys = []
submatrix xs [] = []
submatrix (x:xs) (y:ys) = if (length x) == (length y)
                          then 
                            ((listsubtract (listadd x y)) : (submatrix (xs) (ys)))
                          else
                             error "Matrix dimenstions are not the same"
-- | This function takes a constant of Num class and multiplies a Matrix (List of Lists) by it. e.g multMatrixByC 2 [[1,2],[3,4]] == [[2,4],[6,8]].
multMatrixByC :: (Num a) => a -> [[a]] -> [[a]]
multMatrixByC c [] = []
multMatrixByC c (x:xs) = (Prelude.map (*c) x): (multMatrixByC c xs)

-- | This function takes a Num class tuple and adds the elements together. e.g tupleadd (5,4) == 9
tupleadd :: (Num a) => (a,a) -> a
tupleadd (x,y) = x+y  
-- | This function takes a Num class tuple and subtracts the elements. e.g tuplesub (4,2) == 2
tuplesub :: (Num a) => (a,a) -> a
tuplesub (x,y) = x-y  

-- | This function takes 2 Matrices (List of Lists) and, using matrix multiplication, multiplies them together e.g matrixMult [[1,2],[3,4]] [[2,2], [2,3]] == [[6,8],[14,18]].
matrixMult ::(Num a) => [[a]] -> [[a]] -> [[a]]
matrixMult [] ys = []
matrixMult xs [] = []
matrixMult (x:xs) ys = (matstep2 x ys): (matrixMult xs ys) 

-- | This function helped me in writing matrixMult. Do not use it individually.
matstep2 :: (Num a) => [a] -> [[a]] -> [a]
matstep2 xs [] = []
matstep2 xs ys = (matstep1 xs [head i | i <- ys]) : (matstep2 xs [tail a | a <- ys, (length a) > 1 ])

-- | This function helped me in writing matrixMult. Do not use it individually.
matstep1 ::(Num a) => [a] -> [a] -> a
matstep1 [] [] = 0
matstep1 xs [] = error "Matrices not right size for multiplication"
matstep1 [] ys = error "Matrices not right size for multiplication"
matstep1 (x:xs) (y:ys) = (x*y) + (matstep1 xs ys)    
