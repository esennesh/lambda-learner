module Priors () where

import Calculi
import Control.Monad.Bayes.Class
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

