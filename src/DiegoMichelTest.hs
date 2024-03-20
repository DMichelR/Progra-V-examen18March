module DiegoMichelTest (evalExpr) where

import qualified Data.Map as Map
import ParserBool

type VarAssignments = Map.Map Char Bool

evalExpr :: Expr -> VarAssignments -> Bool
evalExpr (Var c) vars = Map.findWithDefault False c vars
evalExpr (Not expr) vars = not (evalExpr expr vars)
evalExpr (And expr1 expr2) vars = evalExpr expr1 vars && evalExpr expr2 vars
evalExpr (Or expr1 expr2) vars = evalExpr expr1 vars || evalExpr expr2 vars