module Inference where

import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
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

importancePosterior :: MonadSample m => Expr -> Weighted m Expr
importancePosterior val | value val = do
  e <- scoreImportance (expand 0.1 val) priorScore
  likelihood val e
  return e
  where
    exprType = fromJust $ check val Map.empty
    priorScore e = case exprScore Map.empty exprType e of
      Just s -> s
      Nothing -> error $ "Expression of type " ++ (show $ check e Map.empty) ++ " =?= " ++ show exprType ++ " has no score"
importancePosterior _ = error "Observation is not a value"

runWeightedPopulation :: MonadSample m => Population m a -> m [a]
runWeightedPopulation = liftM (map fst) . runPopulation . resampleSystematic

runProgramExpansion :: MonadSample m => Int -> Expr -> m [Expr]
runProgramExpansion n val = runWeightedPopulation population where
  population = spawn n >> posterior
  posterior = fromWeightedList $ (:[]) <$> runWeighted (importancePosterior val)

ioProgramExpansion :: Int -> Expr -> IO [Expr]
ioProgramExpansion n val = sampleIO $ runProgramExpansion n val
