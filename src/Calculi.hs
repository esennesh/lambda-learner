module Calculi where

import Control.Applicative
import Control.Monad.Bayes.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Map as Map

data ExprType = IntTy | BoolTy | StringTy | DoubleTy
              | FuncTy ExprType ExprType deriving (Eq, Show)

data ConstantExpr = IntConstant Int | BoolConstant Bool | StrConstant String
                  | DoubleConstant Double deriving (Eq, Show)

data Expr = Var String | App Expr Expr | Abs (String, ExprType) Expr
          | Flip Expr | Constant ConstantExpr deriving (Eq, Show)

check :: Expr -> Map.Map String ExprType -> Maybe ExprType
check (App func arg) context = check func context >>= \case
  FuncTy argType t -> Just t
  _ -> Nothing
  where
    argType = check arg context
check (Var name) context = Map.lookup name context
check (Flip expr) context = do
  DoubleTy <- check expr context
  return BoolTy
check (Abs (name, argTy) expr) context = do
  resultTy <- check expr (Map.insert name argTy context)
  return (FuncTy argTy resultTy)
check (Constant constant) _ = Just $ case constant of
  IntConstant _ -> IntTy
  BoolConstant _ -> BoolTy
  StrConstant _ -> StringTy
  DoubleConstant _ -> DoubleTy

eval :: MonadSample m => Expr -> Map.Map String Expr -> MaybeT m Expr
eval (App func arg) context = eval func context >>= \case
  Abs (name, _) body -> eval body (Map.insert name arg context)
  _ -> empty
eval (Var name) context = case Map.lookup name context of
  Just e -> return e
  Nothing -> empty
eval (Flip e) context = do
  (Constant (DoubleConstant weight)) <- eval e context
  coin <- bernoulli weight
  return $ Constant (BoolConstant coin)
eval constant@(Constant _) _ = return constant
eval abs@(Abs _ _) _ = return abs
