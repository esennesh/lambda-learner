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

subst :: String -> Expr -> Expr -> Expr
subst name val (Var name') | name == name' = val
subst name val (App f a) = App (subst name val f) (subst name val a)
subst name val (Abs (arg, argType) body) = Abs (arg, argType) body' where
  body' = if name /= arg then subst name val body else body
subst name val (Flip expr) = Flip (subst name val expr)
subst _ _ expr = expr

eval :: MonadSample m => Expr -> MaybeT m Expr
eval (Var _) = empty
eval (App func arg) = eval func >>= \case
  Abs (name, _) body -> eval (subst name arg body)
  _ -> empty
eval abs@(Abs _ _) = return abs
eval (Flip e) = do
  (Constant (DoubleConstant weight)) <- eval e
  coin <- bernoulli weight
  return $ Constant (BoolConstant coin)
eval constant@(Constant _) = return constant

value :: Expr -> Bool
value (Abs binding expr) = True
value (Constant c) = True
value _ = False

runExpr :: MonadSample m => (Expr -> MaybeT m Expr) -> Expr -> m (Maybe Expr)
runExpr evaluator = runMaybeT . evaluator
