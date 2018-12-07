module Priors where

import Calculi
import Control.Monad.Bayes.Class
import Data.Functor.Foldable
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector as V
import Numeric.Log

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
constExpr _ DoubleTy = (Fix . Constant . DoubleConstant) <$> normal 0.0 1.0

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

compatibles :: Map.Map String ExprType -> ExprType -> Map.Map String ExprType
compatibles ctx t = Map.filter (== t) ctx

var :: MonadSample m => Map.Map String ExprType -> ExprType -> Maybe (m Expr)
var ctx t | compats /= Map.empty =
    Just (var_at_index <$> uniformD [0..length compats-1])
  where
    var_at_index index = Fix . Var . fst $ (Map.toList compats !! index)
    compats = compatibles ctx t
var _ _ = Nothing

expr :: MonadSample m => Map.Map String ExprType -> ExprType -> m Expr
expr ctx t = do
  generate <- bernoulli 0.5
  case var ctx t of
    Just v | not generate -> v
    _ -> operator ctx t

exprScore :: Map.Map String ExprType -> ExprType -> Expr -> Maybe (Log Double)
exprScore ctx t (Fix (Var name)) =
  case Map.lookup name ctx of
    Just t' | t == t' -> Just $ 0.5 * 1 / (fromIntegral $ length (compatibles ctx t))
    Nothing -> error ("Unknown variable " ++ name)
exprScore ctx t op = (* 0.5) <$> operatorScore ctx t op

operatorScore :: Map.Map String ExprType -> ExprType -> Expr -> Maybe (Log Double)
operatorScore ctx (FuncTy a b) (Fix (Abs (arg, a') body)) | a == a' = do
  justBody <- exprScore (Map.insert arg a' ctx) b body
  return $ (stringScore arg) * justBody
operatorScore ctx t (Fix (App func arg)) = do
  funcType <- check func ctx
  argType <- check arg ctx
  func' <- exprScore ctx funcType func
  arg' <- exprScore ctx argType arg
  return $ 0.5 * func' * arg'
operatorScore ctx t constant@(Fix (Constant c)) = check constant ctx >>= \t' ->
  if t' == t then Just (0.5 * constScore c) else error ("Constant  " ++ show constant ++ " does not share required type " ++ show t)

constScore :: ConstantExpr -> Log Double
constScore (IntConstant i) = 0.5 ** (fromIntegral i)
constScore (BoolConstant _) = 0.5
constScore (StrConstant str) = stringScore str
constScore (DoubleConstant d) = normalPdf 0.0 1.0 d

stringScore str = (0.5 ** fraclen str) * (1.0/26.0) ** (fraclen str) where
  fraclen arg = fromIntegral $ length arg

sizedValues :: Expr -> [(Expr, Int)]
sizedValues = map (\e -> (e, length (unfix e) + 1)) . values

weightedValues :: Expr -> [(Expr, Double)]
weightedValues expr = map (\(e, s) -> (e, fromIntegral s / total)) $ vals where
  vals = sizedValues expr
  total = fromIntegral . Prelude.sum $ map snd vals

subValue :: MonadSample m => Expr -> m Expr
subValue e = let (vals, weights) = unzip (weightedValues e) in do
  idx <- categorical (V.fromList weights)
  return (vals !! idx)
