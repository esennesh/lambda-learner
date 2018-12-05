module Inference where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Trans.Maybe
import qualified Data.Map as Map
import Data.Maybe

import Calculi
import Priors
import Rules

likelihood :: MonadInfer m => Expr -> Expr -> m Expr
likelihood obsVal e = do
  val <- runMaybeT $ rulesEval e
  case val of
    Just val -> do
      condition (val == obsVal)
      return val
    Nothing -> return e

importancePosterior :: MonadInfer m => Expr -> Traced m Expr
importancePosterior val = do
  let exprType = fromJust $ check val Map.empty
  let prior = expr Map.empty exprType
  let observation = prior >>= \x -> likelihood val x
  let proposal = expand 0.1 val
  importance observation proposal
