module Priors () where

import Calculi
import Control.Monad.Bayes.Class
import qualified Data.Map as Map
import Data.Maybe

constant :: MonadSample m => m ConstantExpr
constant = do
  constructor <- uniformD [1..3]
  case constructor of
    1 -> IntConstant <$> geometric 0.5
    2 -> BoolConstant <$> bernoulli 0.5
    3 -> StrConstant <$> string

string :: MonadSample m => m String
string = undefined

expr :: MonadSample m => Map.Map String Expr -> m Expr
expr context = do
  constructor <- uniformD [1..(length context + 3)]
  if constructor <= length context then do
    variable <- uniformD (Map.keys context)
    return $ Var variable
  else
    case constructor - length context of
      1 -> do
        func <- expr context
        arg <- expr context
        return (App func arg)
      2 -> do
        name <- string
        body <- expr context
        return (Abs name body)
      3 -> do
        constant <- constant
        return (Constant constant)
