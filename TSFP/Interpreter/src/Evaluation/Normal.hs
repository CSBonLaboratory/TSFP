module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import Syntax.Expression
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.Either
import qualified Data.Map as M

{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition
{-
    -- Lab 5: Implementation of eval without monads
eval (Binding sym val) oldCtx = (val, M.insert sym val oldCtx)

eval sym@(Symbol sim) ctx = (M.findWithDefault sym sim ctx , ctx)

eval (Lambda fo bo) ctx = ((Lambda fo bo), ctx)

eval (Application (Lambda x e) e') ctx = let reduced = subst x e' e
                                         in (reduced,ctx)
                                         
eval (Application e e'') ctx = let e' = fst $ eval e ctx 
                                in ((Application e' e''),ctx)

-}

-- Lab 6 : Implementation of eval using monads
--eval expr = runStateT (evalM expr)

-- eval expr ctx = let ExceptT (Identity either) = runStateT (evalM expr) ctx
--                 in case either of
--                     Left _ -> (expr,ctx)
--                     Right (a,s) -> (a,s)
eval = undefined

evalM :: Expression -> Eval Expression
{-
    Lab 6
evalM (Binding sym val) = let enrichedNoResultMonad = modify (M.insert sym val)  -- State ((), Context)
                              noResToVal = \_ -> return val
                          in enrichedNoResultMonad >>= noResToVal

-- get starting state and modify only the returning parameter 
evalM sym@(Symbol sim) = get >>= (\ctx -> return (M.findWithDefault sym sim ctx))

-- get starting state and make returning parameter a Lambda instead of context
evalM lam@(Lambda fo bo) = return lam

-- get starting state and make substitution for returnign parameter. We dont need context for substitution
evalM (Application (Lambda x e) e') = return $ subst x e' e

evalM (Application e e'') = evalM e >>= (\e' -> return ((Application e' e'')))
-}


--Lab 8

-- return a here will behave like ExceptT $ Identity $ a 
evalM (Binding sym val) = StateT $ \ctx -> return (val, M.insert sym val ctx)

evalM sym@(Symbol sim) = do
    ctx <- get
    if M.findWithDefault sym sim ctx == sym
        then do
            lift $ lift $ putStrLn "Exception"
            throwError ("Variable " ++ sim ++ " not found")
        else return $ M.findWithDefault sym sim ctx

evalM lam@(Lambda fo bo) = return lam

evalM (Application (Lambda x e) e' ) = return $ subst x e' e

evalM (Application e e'') = evalM e >>= (\e' -> return ((Application e' e'')))
{-
    -- Lab 6 : another solution for the first 3 cases of evalM
evalM expr = state $ \s -> case expr of
    Binding sym val -> (val, M.insert sym val s)
    sym@(Symbol si) -> (M.findWithDefault sym si s, s)
    lam@(Lambda fo bo) -> (lam, s)
    (Application (Lambda x e) e') -> (subst x e e', s)
-}
