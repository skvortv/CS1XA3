{-|
Module : ExprTest
Description :  imports all modules to be tested
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
-- plus minus div mult cos sin log natexp exp


{-Testing for the eval function in ExprDiff-}
sampleExpr1 :: Expr Double
sampleExpr1 = ((var "x") !+ (var "y")) !/ ((var "x") !+ (var "y")) 

sampleExpr2 :: Expr Double
sampleExpr2 = ((var "x") !- (var "y")) !^ (val 0)

sampleExpr3 :: Expr Double
sampleExpr3 = ((mycos (var "x")) !^ val 2) !+ ((mysin (var "x")) !^ val 2)

sampleExpr4 :: Expr Double
sampleExpr4 = mylog(natexp (var "x"))

test1 :: Double -> Double ->  Bool
test1 x y = eval (Map.fromList [("x",x),("y", y )]) sampleExpr1 == 1

test2 :: Double -> Double -> Bool
test2 x y = eval (Map.fromList [("x",x),("y",1/x)]) sampleExpr2 == 1

test3 :: Double -> Bool
test3 x = abs (eval (Map.fromList [("x",x)]) sampleExpr3) -1 < 0.1

test4 :: Double -> Bool
test4 x = if (abs x) < 700 then 
           abs ((eval (Map.fromList [("x",x)]) sampleExpr4) - x) < 0.1  
         else 
            True 

{-Testing for the simplify function in ExprDiff-}
test5 :: Double -> Bool
test5 x = simplify (Map.fromList [("x",x)]) (var "x" !- val 3 !+ val 0 !- val 0 !+ var "y" ) == val (x-3) !+ var "y" 

test6 :: Double -> Bool
test6 x = simplify (Map.fromList [("x",x)]) ( (var "x" !^ var "y") !/ (var "x" !^ var "y")) == val 1

test7 :: Double -> Bool
test7 x = simplify (Map.fromList [("x",x)]) (var "x" !^ (natexp (var "y"))) == (val x) !^ (natexp ((var "y")))

test8 :: Double -> Double -> Bool
test8 x y = if x < 0
    then True
    else 
      simplify (Map.fromList [("x",x), ("y",y)]) (Log(var "x") !+ Cos(var "y") !- Sin(var "z")) == (val ((log x) + (cos y)) ) !- (Sin(var "z"))

test9 :: Double -> Bool
test9 x = if x == 0
    then True
     else
      simplify (Map.fromList [("x",x)]) ( (matrix [[1,2,3],[1,2,3],[1,2,3]] ) !* var "x" !+ var "y" ) == (matrix [[x,2*x,3*x],[x,2*x,3*x],[x,2*x,3*x]]) !+ var "y"

{-Testing for the partDiff function in ExprDiff-}
