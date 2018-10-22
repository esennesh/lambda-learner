module Calculi where

import qualified Data.Map as Map

data ExprType = IntTy | BoolTy | StringTy | FuncTy ExprType ExprType

data ConstantExpr = IntConstant Int | BoolConstant Bool | StrConstant String
  deriving (Eq, Show)

data Expr = Var String | App Expr Expr | Abs String Expr |
            Constant ConstantExpr deriving (Eq, Show)

eval :: Expr -> Map.Map String Expr -> Maybe Expr
eval (App func arg) context = eval func context >>= \case
  Abs name body -> eval body (Map.insert name arg context)
  _ -> Nothing
eval (Var name) context = Map.lookup name context
eval constant _ = Just constant
