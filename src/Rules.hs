module Rules where

import Calculi
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Functor.Foldable
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Maybe
import Data.Pattern as Pattern
import Priors

type Context t = Map.Map String t
data Rule e m r = Rule {rule :: e -> MaybeT m r}

instance Monad m => Functor (Rule e m) where
  fmap f (Rule r) = Rule $ \e -> f <$> r e

instance Monad m => Applicative (Rule e m) where
  pure x = Rule $ \e -> return x
  (Rule r) <*> (Rule r') = Rule $ \e -> do
    a <- r' e
    f <- r e
    return (f a)

instance Monad m => Alternative (Rule e m) where
  empty = Rule $ \e -> MaybeT (return Nothing)
  (Rule r) <|> (Rule r') = Rule $ \e -> r e <|> r' e

type TypingRule m = Context ExprType -> Rule Expr m ExprType
type RedexRule m = Rule Expr m Expr

step :: Monad m => [Rule e m r] -> e -> MaybeT m r
step rules e = (rule $ asum rules) $ e

applicationTyping :: TypingRule Identity
applicationTyping ctx = Rule $ \e -> case unfix e of
  App func arg -> do
    FuncTy argType t <- rulesCheck ctx func
    argType <- rulesCheck ctx arg
    return t
  _ -> empty

varTyping :: TypingRule Identity
varTyping ctx = Rule $ \e -> case unfix e of
  Var name -> (MaybeT . return) (Map.lookup name ctx)
  _ -> empty

flipTyping :: TypingRule Identity
flipTyping ctx = Rule $ \e -> case unfix e of
  Flip expr -> do
    DoubleTy <- rulesCheck ctx expr
    return BoolTy
  _ -> empty

absTyping :: TypingRule Identity
absTyping ctx = Rule $ \e -> case unfix e of
  Abs (name, argType) body -> do
    bodyType <- rulesCheck (Map.insert name argType ctx) body
    return (FuncTy argType bodyType)
  _ -> empty

constTyping :: TypingRule Identity
constTyping ctx = Rule $ \e -> case unfix e of
  Constant c -> pure (constType c) where
    constType (IntConstant _) = IntTy
    constType (BoolConstant _) = BoolTy
    constType (StrConstant _) = StringTy
    constType (DoubleConstant _) = DoubleTy
  _ -> empty

typingRules = [applicationTyping, varTyping, flipTyping, absTyping, constTyping]

rulesCheck :: Context ExprType -> Expr -> MaybeT Identity ExprType
rulesCheck ctx = step $ map (\rule -> rule ctx) typingRules

applyStepAbs :: Monad m => RedexRule m
applyStepAbs = Rule $ \e -> case unfix e of
  (App (Fix (Abs (argName, argType) body)) arg) | value arg ->
    pure (subst argName arg body)
  _ -> empty

applyStepLeft :: MonadSample m => RedexRule m
applyStepLeft = Rule $ \e -> case unfix e of
  (App func arg) -> do
    func' <- rulesStep func
    return . Fix $ App func' arg
  _ -> empty

applyStepRight :: MonadSample m => RedexRule m
applyStepRight = Rule $ \e -> case unfix e of
  App func arg | value func -> do
    arg' <- rulesStep arg
    return . Fix $ App func arg'
  _ -> empty

flipStepSample :: MonadSample m => RedexRule m
flipStepSample = Rule $ \e -> case unfix e of
  Flip (Fix (Constant (DoubleConstant p))) -> do
    coin <- bernoulli p
    return . Fix $ Constant (BoolConstant coin)
  _ -> empty

flipStepProb :: MonadSample m => RedexRule m
flipStepProb = Rule $ \e -> case unfix e of
  Flip expr -> do
    expr' <- rulesStep expr
    return . Fix $ Flip expr'
  _ -> empty

smallStepRules :: MonadSample m => [RedexRule m]
smallStepRules = [applyStepAbs, applyStepLeft, applyStepRight,
                  flipStepSample, flipStepProb]

rulesStep :: MonadSample m => Expr -> MaybeT m Expr
rulesStep = step smallStepRules

rulesEval :: MonadSample m => Expr -> MaybeT m Expr
rulesEval expr = do
  expr' <- rulesStep expr
  if value expr' then
    return expr'
  else
    rulesEval expr'

-- Note that in general when inverting expressions, we have to preserve their
-- type and value.

type ClauseRule e m r = Clause e (m r)
type ExpansionRule m = ClauseRule Expr m Expr

-- Invert applyStepAbs by factoring a value out of an expression, creating an
-- abstraction.
applyExpandAbs :: MonadSample m => ExpansionRule m
applyExpandAbs = Pattern.var /\ is (\e -> subValues e /= []) ->> \e -> do
  sub <- subValue e
  subType <- return . fromJust $ check sub Map.empty
  varName <- string
  -- What if sub is a value in some contexts inside this expression, but not others?
  let body = replace sub (varExpr varName) e
      appl = app (abstr varName subType body) sub in
    case check appl Map.empty of
      Just _ -> return appl
      Nothing -> error $ "Subexpression abstraction of " ++ show sub ++ " does not preserve type"

applyExpandLeft :: MonadSample m => ExpansionRule m
applyExpandLeft = pattern Pattern.var Pattern.var ->> \func arg -> do
  func' <- expandStep func
  return (app func' arg)
  where
    pattern = mk2 $ \e -> case unfix e of
      App f a | expandable f -> Just (f, a)
      _ -> Nothing

applyExpandRight :: MonadSample m => ExpansionRule m
applyExpandRight = pattern Pattern.var Pattern.var ->> \func arg -> do
  arg' <- expandStep arg
  return (app func arg')
  where
    pattern = mk2 $ \e -> case unfix e of
      App f a | expandable a -> Just (f, a)
      _ -> Nothing

flipExpandSample :: MonadSample m => ExpansionRule m
flipExpandSample = pattern Pattern.var ->> \_ -> do
  p <- uniform 0 1
  return (Calculi.flip (Calculi.constant (DoubleConstant p)))
  where
    pattern = mk1 $ \e -> case unfix e of
      Constant (BoolConstant b) -> Just b
      _ -> Nothing

flipExpandProb :: MonadSample m => ExpansionRule m
flipExpandProb = pattern Pattern.var ->> \prob -> do
  prob' <- expandStep prob
  return (Calculi.flip prob')
  where
    pattern = mk1 $ \e -> case unfix e of
      Flip prob | expandable prob -> Just prob
      _ -> Nothing

expandRules :: MonadSample m => [ExpansionRule m]
expandRules = [applyExpandAbs, applyExpandLeft, applyExpandRight,
               flipExpandSample, flipExpandProb]

expandable :: Expr -> Bool
expandable e = not $ null [r | r <- expandRules', isJust (tryMatch e r)] where
  expandRules' = (expandRules :: [ExpansionRule SamplerIO])

expandableValues :: Expr -> [Expr]
expandableValues = filter expandable . values

expandStep :: MonadSample m => Expr -> m Expr
expandStep e = if numApplicable < 1 then return e else do
    idx <- uniformD [0..numApplicable-1]
    e' <- match e (applicable !! idx)
    case check e' Map.empty of
      Just t' | t == t' -> return e'
      _ -> error $ "Expanded expression " ++ show e' ++ " has different type from original " ++ show e
  where
    t = fromJust $ check e Map.empty
    applicable = [r | r <- expandRules, isJust (tryMatch e r)]
    numApplicable = length applicable

expandSteps :: MonadSample m => Int -> Expr -> m Expr
expandSteps 0 e = return e
expandSteps k e = expandStep e >>= expandSteps (k-1)

expand :: MonadSample m => Double -> Expr -> m Expr
expand p e = (geometric p) >>= \k -> expandSteps k e
