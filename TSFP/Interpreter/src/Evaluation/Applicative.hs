module Evaluation.Applicative where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M

{-|
    Small-step applicative-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition
eval (Binding sym val) oldCtx = (val, M.insert sym val oldCtx)

eval sym@(Symbol s) ctx = (M.findWithDefault sym s ctx , ctx)

eval (Lambda fo bo) ctx = ((Lambda fo bo), ctx)

eval (Application lbd@(Lambda x e'') e@(Symbol _)) ctx = let e' = fst $ eval e ctx
                                                         in ((Application lbd e'),ctx)
eval (Application lbd@(Lambda x e'') e@(Application _ _)) ctx = let e' = fst $ eval e ctx
                                                                in ((Application lbd e'),ctx)
eval (Application (Lambda x e) e'') ctx = let e' = subst x e'' e
                                          in (e',ctx)
eval (Application e e'') ctx = let e' = fst $ eval e ctx
                               in ((Application e' e''),ctx)

 