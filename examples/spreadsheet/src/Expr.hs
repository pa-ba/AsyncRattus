module Expr (
    Expr(..),
    VarEnv,
    emptyVarEnv,
    eval,
    evalMaybe,
) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, empty)
import Control.Monad (liftM2)
import Data.Maybe (fromJust)
import qualified Rattus.Strict as Strict
import Rattus.Strict (List(..), (+++))

type VarEnv = Map String Int

data Expr = Plus Expr Expr   |
            Minus Expr Expr  |
            Times Expr Expr  |
            Divide Expr Expr |
            Negate Expr      |
            Number Int       |
            Var String deriving (Eq, Show)

emptyVarEnv :: VarEnv
emptyVarEnv = empty

applyBinOp :: Monad m => (a -> a -> a) -> m a -> m a -> m a
applyBinOp f e e' = liftM2 f e e'

evalMaybe :: Expr -> VarEnv -> Maybe Int
evalMaybe (Plus e e') varEnv = applyBinOp (+) (evalMaybe e varEnv) (evalMaybe e' varEnv)
evalMaybe (Minus e e') varEnv = applyBinOp (-) (evalMaybe e varEnv) (evalMaybe e' varEnv)
evalMaybe (Times e e') varEnv = applyBinOp (*) (evalMaybe e varEnv) (evalMaybe e' varEnv)
evalMaybe (Divide e e') varEnv = do
    eRes <- evalMaybe e varEnv
    e'Res <- evalMaybe e' varEnv
    if e'Res == 0
        then Nothing 
        else return $ eRes `div` e'Res
evalMaybe (Negate e) varEnv = evalMaybe e varEnv >>= return . (0-)
evalMaybe (Number i) varEnv = Just i
evalMaybe (Var name) varEnv = lookup name varEnv 

eval :: Expr -> VarEnv -> Int
eval = fmap fromJust . evalMaybe

depends :: Expr -> List String
depends (Plus e e') = depends e +++ depends e'
depends (Minus e e') = depends e +++ depends e'
depends (Times e e') = depends e +++ depends e'
depends (Divide e e') = depends e +++ depends e'
depends (Negate e) = depends e
depends (Number _) = Nil
depends (Var s) = s :! Nil