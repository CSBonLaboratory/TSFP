module Evaluation.Big where

import Syntax.Expression
import Data.List
import Data.Tuple (swap)
import Control.Monad.State

{-|
    Big-step evaluation of a given expression, within a given context.
    The evaluation should stop when either the value is reached,
    or the expression cannot be reduced further.
    
    The first argument is the small-step evaluation function.
-}
evalBig :: (Expression -> Context -> (Expression, Context))  -- ^ Small-stepper
        -> Expression             -- ^ Expression to be evaluated
        -> Context                -- ^ Context where the evaluation takes place
        -> (Expression, Context)  -- ^ Evaluation result,
                                  --   together with a possibly enriched context
                                  --   in case of definition
{-
    --Lab 4: evalBig without monads                     
evalBig smallStepper e ctx = let (nextE, nextCtx) = smallStepper e ctx
                             in if nextE == e then (nextE,nextCtx) else evalBig smallStepper nextE nextCtx
-}


    -- Lab 5: evalBig with monads

{- We need to transform smallStepper :: Expression -> Context -> (Expression,Context)
   to smallStepperM :: Expression -> Eval Expression
   So we need to transform Context -> (Expression, Context) to State {runState :: Context -> (Expression,Context)}
   We accomplish it by a partial application of smallStepper and then building the state monad using the partial result
-}
evalBig smallStepper expr ctx = runState (evalBigM smallStepperM expr) ctx where
    smallStepperM = \e -> let ctxToPair = smallStepper e in state $ ctxToPair  

evalBigM :: (Expression -> Eval Expression) -> Expression -> Eval Expression
evalBigM smallStepperM expr = let nextEval = smallStepperM expr
                                  testFinish = (\e -> if e == expr then nextEval else evalBigM smallStepperM e)
                             in nextEval >>= testFinish
                        
{-|
    Big-step evaluation of a list of expressions, starting with
    the given context and using it throughout the entire list,
    for propagating the encountered definitions.
    
    The first argument is the small-step evaluation function.
-}

{-
    Lab 4 : Type inference prototype
mapAccumL :: (a -> b -> (a,c)) -> a -> [b] -> (a, [c])

(a, [c]) => ([Expression], Context) => 

a = Context, c = Expression

b = Expression

-}


evalList :: (Expression -> Context -> (Expression, Context))
         -> [Expression]
         -> Context
         -> ([Expression], Context)

{-
    Lab 4: Implementation of evalList without monads
evalList bigStepper exprs ctx = let res = mapAccumL (\context expression -> swap $ bigStepper expression context) ctx exprs
                                in swap res
-}

evalList bigStepper exprs ctx = runState (evalListM bigStepperM exprs) ctx where
    bigStepperM = \e -> let ctxToPair = bigStepper e in state $ ctxToPair 

evalListM :: (Expression -> Eval Expression) -> [Expression] -> Eval [Expression]
evalListM smallStepper = mapM (evalBigM smallStepper) 
