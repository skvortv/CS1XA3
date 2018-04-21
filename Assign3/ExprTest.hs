{-|
Module : ExprTest
Description :  Imports all modules to be tested. The functions eval, simplify, partDiff, and parseExprDouble all some test cases to quickCheck over. For the Exp data 
               Constructor partDiff does not work so only use Natexp or multiplication for accurate answers when doing exponentiation.
Copyright    : (c) Seva Skvortsov @2018
License      : WTFPL
Maintainer   : seva.sk@gmail.com
Stability    : experimental
Portability  : POSIX -}



module ExprTest where 

import Data.Map as Map

import ExprType
import ExprDiff
import ExprParser
import ExprPretty

import Test.QuickCheck


{-Testing for the eval function in ExprDiff-}

sampleExpr1 :: Expr Double
sampleExpr1 = ((var "x") !+ (var "y")) !/ ((var "x") !+ (var "y")) 

sampleExpr2 :: Expr Double
sampleExpr2 = ((var "x") !- (var "y")) !^ (val 0)

sampleExpr3 :: Expr Double
sampleExpr3 = ((mycos (var "x")) !^ val 2) !+ ((mysin (var "x")) !^ val 2)

sampleExpr4 :: Expr Double
sampleExpr4 = mylog(natexp (var "x"))

testE1 :: Double -> Double ->  Bool
testE1 x y = eval (Map.fromList [("x",x),("y", y )]) sampleExpr1 == 1

testE2 :: Double -> Double -> Bool
testE2 x y = eval (Map.fromList [("x",x),("y",1/x)]) sampleExpr2 == 1

testE3 :: Double -> Bool
testE3 x = abs (eval (Map.fromList [("x",x)]) sampleExpr3) -1 < 0.1

testE4 :: Double -> Bool
testE4 x = if (abs x) < 700 then 
           abs ((eval (Map.fromList [("x",x)]) sampleExpr4) - x) < 0.1  
         else 
            True 

{-Testing for the simplify function in ExprDiff-}

testS1 :: Double -> Bool
testS1 x = simplify (Map.fromList [("x",x)]) (var "x" !- val 3 !+ val 0 !- val 0 !+ var "y" ) == val (x-3) !+ var "y" 

testS2 :: Double -> Bool
testS2 x = simplify (Map.fromList [("x",x)]) ( (var "x" !^ var "y") !/ (var "x" !^ var "y")) == val 1

testS3 :: Double -> Bool
testS3 x = simplify (Map.fromList [("x",x)]) (var "x" !^ (natexp (var "y"))) == (val x) !^ (natexp ((var "y")))

testS4 :: Double -> Double -> Bool
testS4 x y = if x < 0
    then True
    else 
      simplify (Map.fromList [("x",x), ("y",y)]) (Log(var "x") !+ Cos(var "y") !- Sin(var "z")) == (val ((log x) + (cos y)) ) !- (Sin(var "z"))

testS5 :: Double -> Bool
testS5 x = if x == 0
    then True
     else
      simplify (Map.fromList [("x",x)]) ( (matrix [[1,2,3],[1,2,3],[1,2,3]] ) !* var "x" !+ var "y" ) == (matrix [[x,2*x,3*x],[x,2*x,3*x],[x,2*x,3*x]]) !+ var "y"

testS6 :: Double -> Bool
testS6 x = if x == 0
           then simplify (Map.fromList []) ((matrix [[1,2,3],[1,2,3]] !* matrix [[1,1,1,1],[2,2,2,2],[3,3,3,3]]) !* val x) == val 0
           else
            simplify (Map.fromList []) ((matrix [[1,2,3],[1,2,3]] !* matrix [[1,1,1,1],[2,2,2,2],[3,3,3,3]]) !* val x) == Matrix [[14*x,14*x,14*x,14*x],[14*x,14*x,14*x,14*x]]

{-Testing for the partDiff function in ExprDiff-}

testD1 :: String -> Double -> Bool
testD1 s a = partDiff s (val a) == (val 0)

testD2 :: String -> Double -> Bool
testD2 s a = partDiff s (var s) !+ (val a) == (val a !+ val 1)

testD3 :: String -> Double -> Bool 
testD3 s a = if s == "y"
              then partDiff s ((var s) !- (var "y")) !* (val a) ==  val 0
              else
                partDiff s ((var s) !- (var "y")) !* (val a) == val a

testD4 :: String -> Double -> Bool
testD4 s a = if s == "y" 
             then True
             else
               partDiff s  ((var s) !/ (var "y")) == Div (Sub (Mult (Const 1.0) (Var "y")) (Mult (Var s) (Const 0.0))) (Exp (Var "y") (Const 2.0))

testD5 :: String -> Double -> Bool
testD5 s a = partDiff s (mylog (natexp (val a))) == val 0

testD6 :: String -> Double -> Bool
testD6 s a = if s == "y"
             then True
             else 
              partDiff s (mysin (mycos (var "y")) ) == Mult (Mult (Const (-1.0)) (Mult (Const 0.0) (Sin (Var "y")))) (Cos (Cos (Var "y")))

{-Testing for the partDiff function in ExprDiff-}

testP1 :: Bool
testP1 = (parseExprDouble "((1+2-3*4)/  5) ^ ( Cos(Sin(Log(e^(XxXYx) ))))" ) == Exp (Div (Sub (Add (Const 1.0) (Const 2.0)) (Mult (Const 3.0) (Const 4.0))) (Const 5.0)) (Cos (Sin (Log (NatExp (Var "XxXYx")))))

testP2 :: Bool
testP2 = parseExprDouble "(5Cosx)" /= Mult (Const 5.0) (Cos (Var "x"))

testP3 :: Bool
testP3 = parseExprDouble "5*Cos(x)*[1,2,3;1,2,3;1,2,3]" == Mult (Const 5.0) (Cos (Mult (Var "x") (Matrix [[1.0,2.0,3.0],[1.0,2.0,3.0],[1.0,2.0,3.0]])))

 