module Priors () where

import Calculi
import Control.Monad.Bayes.Class
import Data.Functor.Foldable
import qualified Data.Map as Map
import Data.Maybe

exprType :: MonadSample m => m ExprType
exprType = do
  constructor <- uniformD [1..4]
  case constructor of
    1 -> return IntTy
    2 -> return BoolTy
    3 -> return StringTy
    4 -> do
      arg <- exprType
      result <- exprType
      return (FuncTy arg result)

constant :: MonadSample m => m ConstantExpr
constant = do
  constructor <- uniformD [1..3]
  case constructor of
    1 -> IntConstant <$> geometric 0.5
    2 -> BoolConstant <$> bernoulli 0.5
    3 -> StrConstant <$> string

char :: MonadSample m => m Char
char = uniformD ['a'..'z']

string :: MonadSample m => m String
string = do
  flip <- bernoulli 0.5
  c <- char
  if flip then
    string >>= \str -> return (c:str)
  else
    return [c]

constExpr :: MonadSample m => Map.Map String ExprType -> ExprType -> m Expr
constExpr _ IntTy = (Fix . Constant . IntConstant) <$> geometric 0.5
constExpr _ BoolTy = (Fix . Constant . BoolConstant) <$> bernoulli 0.5
constExpr _ StringTy = (Fix . Constant . StrConstant) <$> string

operator :: MonadSample m => Map.Map String ExprType -> ExprType -> m Expr
operator ctx (FuncTy a b) = do
  arg <- string
  body <- expr (Map.insert arg a ctx) b
  return . Fix $ Abs (arg, a) body
operator ctx t = do
  generate_constant <- bernoulli 0.5
  if generate_constant then
    constExpr ctx t
  else do
    ta <- exprType
    a <- expr ctx ta
    func <- operator ctx (FuncTy ta t)
    return . Fix $ App func a

var :: MonadSample m => Map.Map String ExprType -> ExprType -> Maybe (m Expr)
var ctx t | compatibles /= Map.empty =
    Just (var_at_index <$> uniformD [0..length compatibles-1])
  where
    var_at_index index = Fix . Var . fst $ (Map.toList compatibles !! index)
    compatibles = Map.filter (== t) ctx
var _ _ = Nothing

expr :: MonadSample m => Map.Map String ExprType -> ExprType -> m Expr
expr ctx t = do
  generate <- bernoulli 0.5
  case var ctx t of
    Just v | generate -> v
    _ -> operator ctx t

sizedValues :: Expr -> [(Expr, Int)]
sizedValues = map (\e -> (e, length (unfix e) + 1)) . values

weightedValues :: Expr -> [(Expr, Double)]
weightedValues expr = map (\(e, s) -> (e, fromIntegral s / total)) $ vals where
  vals = sizedValues expr
  total = fromIntegral . sum $ map snd vals
