module ExprTest where 

import Data.Map as Map

import ExprType
import ExprDiff
import ExprParser
import ExprPretty

import Test.QuickCheck



sampleExpr1 = (var "x") !+ (var "y") !* (val 2.0)

