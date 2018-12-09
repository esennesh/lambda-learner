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

importancePosterior :: MonadSample m => [Expr] -> Weighted m Expr
importancePosterior vals | (all value vals) && (isJust exprTypeMaybe) = do
  bias <- uniform 0.0 1.0
  val <- uniformD vals
  e <- scoreImportance (expand bias val) priorScore
  foldr1 (>>) [likelihood val e | val <- vals]
  return e
  where
    exprType = fromJust $ exprTypeMaybe
    exprTypeMaybe = foldr1 eqAnd [check val Map.empty | val <- vals]
    eqAnd :: Eq a => Maybe a -> Maybe a -> Maybe a
    eqAnd (Just x) (Just x') | x == x' = Just x
    eqAnd _ _ = Nothing
    priorScore e = case exprScore Map.empty exprType e of
      Just s -> s
      Nothing -> error $ "Expression of type " ++ (show $ check e Map.empty) ++ " =?= " ++ show exprType ++ " has no score"
importancePosterior vals | any (not . value) vals =
  error "Observation is not a value"
importancePosterior vals = error "Incompatible observation types"

runWeightedPopulation :: MonadSample m => Population m a -> m [a]
runWeightedPopulation = liftM (map fst) . runPopulation . resampleSystematic

runProgramExpansion :: MonadSample m => Int -> [Expr] -> m [Expr]
runProgramExpansion n vals = runWeightedPopulation population where
  population = spawn n >> posterior
  posterior = fromWeightedList $ (:[]) <$> runWeighted (importancePosterior vals)

ioProgramExpansion :: Int -> [Expr] -> IO [Expr]
ioProgramExpansion n vals = sampleIO $ runProgramExpansion n vals
