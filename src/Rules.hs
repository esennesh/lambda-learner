module Rules where

import Calculi
import Control.Applicative
import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Functor.Identity
import qualified Data.Map as Map

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
applicationTyping ctx = Rule $ \case
  App func arg -> do
    FuncTy argType t <- rulesCheck ctx func
    argType <- rulesCheck ctx arg
    return t
  _ -> empty

varTyping :: TypingRule Identity
varTyping ctx = Rule $ \case
  Var name -> (MaybeT . return) (Map.lookup name ctx)
  _ -> empty

flipTyping :: TypingRule Identity
flipTyping ctx = Rule $ \case
  Flip expr -> do
    DoubleTy <- rulesCheck ctx expr
    return BoolTy
  _ -> empty

absTyping :: TypingRule Identity
absTyping ctx = Rule $ \case
  Abs (name, argType) body -> do
    bodyType <- rulesCheck (Map.insert name argType ctx) body
    return (FuncTy argType bodyType)
  _ -> empty

constTyping :: TypingRule Identity
constTyping ctx = Rule $ \case
  Constant c -> pure (constType c) where
    constType (IntConstant _) = IntTy
    constType (BoolConstant _) = BoolTy
    constType (StrConstant _) = StringTy
    constType (DoubleConstant _) = DoubleTy
  _ -> empty

typingRules = [applicationTyping, varTyping, flipTyping, absTyping, constTyping]

rulesCheck :: Context ExprType -> Expr -> MaybeT Identity ExprType
rulesCheck ctx = step $ map (\rule -> rule ctx) typingRules
