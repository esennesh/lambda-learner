module Priors where

import Calculi
import Control.Monad.Bayes.Class
import Data.Functor.Foldable
import Data.List
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
  constructor <- uniformD [1..4]
  case constructor of
    1 -> IntConstant <$> geometric 0.5
    2 -> BoolConstant <$> bernoulli 0.5
    3 -> StrConstant <$> string
    4 -> DoubleConstant <$> normal 0.0 1.0

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

abstraction :: MonadSample m => Map.Map String ExprType -> ExprType ->
               ExprType -> Int -> m Expr
abstraction ctx a b i = do
  arg <- string
  body <- sizedExpr (Map.insert arg a ctx) b (i-1)
  return . Fix $ Abs (arg, a) body

constExpr :: MonadSample m => Map.Map String ExprType -> ExprType -> m Expr
constExpr _ IntTy = (Fix . Constant . IntConstant . (+ 1)) <$> geometric 0.5
constExpr _ BoolTy = (Fix . Constant . BoolConstant) <$> bernoulli 0.5
constExpr _ StringTy = (Fix . Constant . StrConstant) <$> string
constExpr _ DoubleTy = (Fix . Constant . DoubleConstant) <$> uniform 0.0 1.0
constExpr ctx (FuncTy a b) = abstraction ctx a b 0

application :: MonadSample m => Map.Map String ExprType -> ExprType -> Int ->
               m Expr
application ctx t i = do
  ta <- exprType
  a <- sizedExpr ctx ta (i-1)
  func <- sizedExpr ctx (FuncTy ta t) (i-1)
  return . Fix $ App func a

arithmeticBinOp :: MonadSample m => Map.Map String ExprType -> ExprType ->
                   Int -> m Expr
arithmeticBinOp ctx t i | t == IntTy || t == DoubleTy = do
  op <- uniformD [Plus, Minus, Times, Divide]
  left <- sizedExpr ctx t (i-1)
  right <- sizedExpr ctx t (i-1)
  return . Fix $ BinOp (Arith op) left right

operator :: MonadSample m => Map.Map String ExprType -> ExprType -> Int ->
            m Expr
operator ctx (FuncTy a b) i = abstraction ctx a b i
operator ctx t i | t == IntTy || t == DoubleTy = do
  generate_case <- uniformD [0..2]
  case generate_case of
    0 -> constExpr ctx t
    1 -> application ctx t i
    2 -> arithmeticBinOp ctx t i
operator ctx BoolTy i = do
  generate_case <- uniformD [0..2]
  case generate_case of
    0 -> constExpr ctx BoolTy
    1 -> application ctx BoolTy i
    2 -> do
      prob <- uniform 0.0 1.0
      return . Fix $ Flip (Fix $ Constant (DoubleConstant prob))
operator ctx StringTy i = do
  generate_case <- uniformD [0..1]
  case generate_case of
    0 -> constExpr ctx StringTy
    1 -> application ctx StringTy i

compatibles :: Map.Map String ExprType -> ExprType -> Map.Map String ExprType
compatibles ctx t = Map.filter (== t) ctx

var :: MonadSample m => Map.Map String ExprType -> ExprType -> Maybe (m Expr)
var ctx t | compats /= Map.empty =
    Just (var_at_index <$> uniformD [0..length compats-1])
  where
    var_at_index index = Fix . Var . fst $ (Map.toList compats !! index)
    compats = compatibles ctx t
var _ _ = Nothing

sizedExpr :: MonadSample m => Map.Map String ExprType -> ExprType -> Int ->
             m Expr
sizedExpr ctx t i = do
  let numCompatibles = length (compatibles ctx t)
  fresh_chance <- uniformD [0..numCompatibles]
  case var ctx t of
    Just v | fresh_chance < numCompatibles -> v
    _ | i > 0 -> operator ctx t i
    _ -> constExpr ctx t

expr :: MonadSample m => Map.Map String ExprType -> ExprType -> m Expr
expr ctx t = uniform 0.0 1.0 >>= geometric >>= sizedExpr ctx t

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
  return $ (caseProb t) * func' * arg'
  where
    caseProb DoubleTy = 1/3
    caseProb IntTy = 1/3
    caseProb BoolTy = 1.0/3.0
    caseProb _ = 0.5
operatorScore ctx t constant@(Fix (Constant c)) = check constant ctx >>= \t' ->
  if t' == t then
    Just (caseProb t * constScore c)
  else
    error ("Constant  " ++ show constant ++ " does not share required type " ++ show t)
  where
    caseProb DoubleTy = 1/3
    caseProb IntTy = 1/3
    caseProb BoolTy = 1.0/3.0
    caseProb _ = 0.5
operatorScore ctx BoolTy (Fix (Flip weight)) = do
  weightScore <- exprScore ctx DoubleTy weight
  return $ 1.0/3.0 * weightScore
operatorScore ctx IntTy (Fix (BinOp (Arith _) left right)) = do
  leftScore <- exprScore ctx IntTy left
  rightScore <- exprScore ctx IntTy right
  return $ 1/3 * 1/4 * leftScore * rightScore
operatorScore ctx DoubleTy (Fix (BinOp (Arith _) left right)) = do
  leftScore <- exprScore ctx DoubleTy left
  rightScore <- exprScore ctx DoubleTy right
  return $ 1/3 * 1/4 * leftScore * rightScore
operatorScore _ _ _ = Nothing

constScore :: ConstantExpr -> Log Double
constScore (IntConstant i) = 0.5 ** (fromIntegral i)
constScore (BoolConstant _) = 0.5
constScore (StrConstant str) = stringScore str
constScore (DoubleConstant d) = Exp $ log d

stringScore str = (0.5 ** fraclen str) * (1.0/26.0) ** (fraclen str) where
  fraclen arg = fromIntegral $ length arg

sizedSubValues :: Expr -> [(Expr, Int)]
sizedSubValues = map (\e -> (e, length (unfix e) + 1)) . nub . subValues

weightedSubValues :: Expr -> [(Expr, Double)]
weightedSubValues expr = map (\(e, s) -> (e, fromIntegral s / total)) $ vals where
  vals = sizedSubValues expr
  total = fromIntegral . Prelude.sum $ map snd vals

subValue :: MonadSample m => Expr -> m Expr
subValue e = let (vals, weights) = unzip (weightedSubValues e) in do
  idx <- categorical (V.fromList weights)
  return (vals !! idx)
