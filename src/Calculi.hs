module Calculi where

import qualified Data.Map as Map

data ExprType = IntTy | BoolTy | StringTy | FuncTy ExprType ExprType deriving
  (Eq, Show)

data ConstantExpr = IntConstant Int | BoolConstant Bool | StrConstant String
  deriving (Eq, Show)

data Expr = Var String | App Expr Expr | Abs String Expr |
            Constant ConstantExpr deriving (Eq, Show)

check :: Expr -> Map.Map String ExprType -> Maybe ExprType
check (App func arg) context = check func context >>= \case
  FuncTy argType t -> Just t
  _ -> Nothing
  where
    argType = check arg context
check (Var name) context = Map.lookup name context
check (Constant constant) _ = Just $ case constant of
  IntConstant _ -> IntTy
  BoolConstant _ -> BoolTy
  StrConstant _ -> StringTy

eval :: Expr -> Map.Map String Expr -> Maybe Expr
eval (App func arg) context = eval func context >>= \case
  Abs name body -> eval body (Map.insert name arg context)
  _ -> Nothing
eval (Var name) context = Map.lookup name context
eval constant _ = Just constant
