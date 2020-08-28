module Hutton where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval expr =
  case expr of
    Lit i ->
      i
    Add expr1 expr2 ->
      (eval expr1) + (eval expr2)

printExpr :: Expr -> String
printExpr expr =
  case expr of
    Lit i ->
      show i
    Add expr1 expr2 ->
      (printExpr expr1) ++ " + " ++ (printExpr expr2)
