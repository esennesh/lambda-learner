module Calculi where

import Control.Applicative
import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Foldable
import qualified Data.Map as Map
import Data.Maybe

data ExprType = IntTy | BoolTy | StringTy | DoubleTy
              | FuncTy ExprType ExprType deriving (Eq, Show)

data ConstantExpr = IntConstant Int | BoolConstant Bool | StrConstant String
                  | DoubleConstant Double deriving (Eq, Show)

data ExprF f = Var String | App f f | Abs (String, ExprType) f | Flip f
             | Constant ConstantExpr deriving (Eq, Foldable, Functor, Show)
type PartialExpr = Fix (Compose Maybe ExprF)
type Expr = Fix ExprF

instance Eq1 ExprF where
  liftEq _ (Var v) (Var v') = v == v'
  liftEq eq (App f a)  (App f' a') = (eq f f') && (eq a a')
  liftEq eq (Abs (_, t) b) (Abs (_, t') b') = t == t' && eq b b'
  liftEq eq (Flip e) (Flip e') = eq e e'
  liftEq _ (Constant c) (Constant c') = c == c'
  liftEq _ _ _ = False

instance Show1 ExprF where
  liftShowsPrec _ _ d (Var v) = showsUnaryWith showsPrec "Var" d v
  liftShowsPrec sp _ d (App f f') = showsBinaryWith sp sp "App" d f f'
  liftShowsPrec sp _ d (Abs tup f) =
    showString ("Abs " ++ show tup) . showChar ' ' . sp 11 f
  liftShowsPrec sp _ d (Flip f) = showsUnaryWith sp "Flip" d f
  liftShowsPrec _ _ d (Constant c) = showsUnaryWith showsPrec "Constant" d c

varExpr :: String -> Expr
varExpr = Fix . Var

app :: Expr -> Expr -> Expr
app x y = Fix $ App x y

abstr :: String -> ExprType -> Expr -> Expr
abstr arg argType body = Fix $ Abs (arg, argType) body

flip :: Expr -> Expr
flip = Fix . Flip

constant :: ConstantExpr -> Expr
constant = Fix . Constant

check :: Expr -> Map.Map String ExprType -> Maybe ExprType
check (Fix (App func arg)) context = check func context >>= \case
  FuncTy argType t -> Just t
  _ -> Nothing
  where
    argType = check arg context
check (Fix (Var name)) context = Map.lookup name context
check (Fix (Flip expr)) context = do
  DoubleTy <- check expr context
  return BoolTy
check (Fix (Abs (name, argTy) expr)) context = do
  resultTy <- check expr (Map.insert name argTy context)
  return (FuncTy argTy resultTy)
check (Fix (Constant constant)) _ = Just $ case constant of
  IntConstant _ -> IntTy
  BoolConstant _ -> BoolTy
  StrConstant _ -> StringTy
  DoubleConstant _ -> DoubleTy

subst :: String -> Expr -> Expr -> Expr
subst name val = para $ \case
  Var name' | name == name' -> val
  Abs (arg, argType) (body, body') -> Fix $ Abs (arg, argType) body'' where
    body'' = if name /= arg then body' else body
  expr -> Fix $ fmap snd expr

replace :: Expr -> Expr -> Expr -> Expr
replace sub replacement = cata $ \e ->
  if e == unfix sub then replacement else Fix e

eval :: MonadSample m => Expr -> MaybeT m Expr
eval = para $ \case
  Var _ -> empty
  App (_, func) (_, arg) -> do
    (Fix (Abs (name, _) body)) <- func
    argVal <- arg
    eval (subst name argVal body)
  Abs (name, argType) (body, _) -> return (Fix $ Abs (name, argType) body)
  Flip (_, weightExpr) -> do
    Fix (Constant (DoubleConstant weight)) <- weightExpr
    coin <- bernoulli weight
    return . Fix $ Constant (BoolConstant coin)
  Constant c -> return . Fix $ (Constant c)

contextualValue :: Map.Map String ExprType -> Expr -> Bool
contextualValue ctx (Fix (Abs (a, ta) b)) =
  contextualValue (Map.insert a ta ctx) b
contextualValue _ (Fix (Constant c)) = True
contextualValue ctx (Fix (Var name)) = isJust $ Map.lookup name ctx
contextualValue _ _ = False

value :: Expr -> Bool
value = contextualValue Map.empty

values :: Expr -> [Expr]
values = para $ \e -> let expr = Fix $ fmap fst e in
  if value expr then
    expr:(vals $ fmap snd e)
  else
    vals $ fmap snd e
  where
    vals (App vs vs') = vs ++ vs'
    vals (Abs _ vs) = vs
    vals (Flip vs) = vs
    vals _ = []

subValues :: Expr -> [Expr]
subValues e = filter (/= e) $ values e

runExpr :: MonadSample m => (Expr -> MaybeT m Expr) -> Expr -> m (Maybe Expr)
runExpr evaluator = runMaybeT . evaluator
