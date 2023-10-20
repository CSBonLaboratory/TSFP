module Typing.Unification where

import Typing.Type
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

{-
    A monad for solving unification constraints. It is composed of:
    * a state monad transformer, for maintaining the substitution
    * an exception monad, for signaling unification errors.
-}
type Unif = StateT Substitution (Except String)

runUnif :: Unif a -> Substitution -> Either String (a, Substitution)
runUnif ops subst = runExcept $ runStateT ops subst

{-|
    Obtains the end of the binding chain for the given type.
    The search ends when either of the following is reached:
    
    * an unbound type variable
    
    * a function type.
-}
chainEnd :: Type       -- ^ Type to look up
         -> Unif Type  -- ^ Chain end
chainEnd aType@(TypeVar a) = do
    s <- get
    case (M.findWithDefault aType a s) of
        nextType@(TypeVar x) -> if x == a then return aType else chainEnd nextType
        func@(Arrow _ _) -> return func

chainEnd func@(Arrow _ _) = return func

{-|
    Returns true if a type variable does NOT appear in the given type.
    The type variable is assumed FREE within the substitution.
-}
occCheck :: String     -- ^ Type variable to check for occurrence
         -> Type       -- ^ Type to look in
         -> Unif Bool  -- ^ True if the type variable does NOT occur
occCheck freeVarTypeName ty@(TypeVar t) = do
    if t == freeVarTypeName 
        then return False 
        else 
            do
                tEnd <- chainEnd ty 
                case tEnd of
                    (Arrow a b) -> do
                        leftOcc <- occCheck freeVarTypeName a
                        rightOcc <- occCheck freeVarTypeName b
                        return $ leftOcc && rightOcc
                    (TypeVar x) -> if x == freeVarTypeName then return False else return True

occCheck freeVarTypeName func@(Arrow a b) = do
    leftOcc <- occCheck freeVarTypeName a
    rightOcc <- occCheck freeVarTypeName b
    return $ leftOcc && rightOcc
    
    

{-|
    Unifies two type expressions.
-}
unify :: Type     -- ^ First type
      -> Type     -- ^ Second type
      -> Unif ()  -- ^ () if the types unify or an exception otherwise
unify (TypeVar a) bType@(TypeVar b) = do
    s <- get 
    res <- occCheck a bType
    if res == True
        then put $ M.insert a bType s
        else lift $ throwError $ "Illegal occurence of " ++ a ++ "in " ++ b

unify (Arrow a b) rightFunc@(Arrow c d) = (unify a c) >> (unify b d)

unify (TypeVar a) func@(Arrow b c) = do
    s <- get
    ok <- occCheck a func
    if ok == False
        then lift $ throwError $ "Illegal occurence of " ++ a ++ "in " ++ (show func)
        else put $ M.insert a func s

unify func@(Arrow b c) typeA@(TypeVar a) = unify typeA func 



{-|
    Applies the substitution to a type expression.
-}
applySubst :: Type       -- ^ Target type
           -> Unif Type  -- ^ Resulting type
applySubst typeA@(TypeVar a) = do
    chEnd <- chainEnd typeA
    case chEnd of
        (Arrow b c) -> return (Arrow (applySubst b) (applySubst c))
        typeX@(TypeVar x) -> return typeX

applySubst (Arrow a b) = applySubst (Arrow (applySubst a) (applySubst b))



-- My tests
chainEndTest1 = runUnif (chainEnd (TypeVar "a")) (M.fromList [])
chainEndTest2 = runUnif (chainEnd (TypeVar "a")) (M.fromList [("a",(TypeVar "b"))]) 
chainEndTest3 = runUnif (chainEnd (TypeVar "a")) (M.fromList [("a", (TypeVar "b")),("b", (TypeVar "c"))])
chainEndTest4 = runUnif (chainEnd (TypeVar "a")) (M.fromList [("a", (TypeVar "b")),("b", (TypeVar "c")),("c", (Arrow (TypeVar "d") (TypeVar "e")))])
chainEndTest5 = runUnif (chainEnd (TypeVar "a")) (M.fromList [("a", (TypeVar "b")),("b", (TypeVar "c")),("c", (Arrow (TypeVar "d") (TypeVar "e"))),("d", (TypeVar "f"))])

bigSubst = M.fromList [("a", (TypeVar "b")),("b", (TypeVar "c")), ("c", (Arrow (TypeVar "d") (TypeVar "e")))]

occCheckTest1 = runUnif (occCheck "a" (TypeVar "a")) (M.fromList [])
occCheckTest2 = runUnif (occCheck "a" (TypeVar "b")) (M.fromList [])
occCheckTest3 = runUnif (occCheck "a" (Arrow (TypeVar "a") (TypeVar "b"))) (M.fromList [])
occCheckTest4 = runUnif (occCheck "a" (Arrow (TypeVar "b") (TypeVar "a"))) (M.fromList [])
occCheckTest5 = runUnif (occCheck "z" (TypeVar "a")) bigSubst
occCheckTest6 = runUnif (occCheck "d" (TypeVar "a")) bigSubst
occCheckTest7 = runUnif (occCheck "e" (TypeVar "a")) bigSubst
occCheckTest8 = runUnif (occCheck "a" (TypeVar "b")) (M.insert "e" (Arrow (TypeVar "a") (TypeVar "x")) $ M.insert "d" (Arrow (TypeVar "y") (TypeVar "z")) bigSubst)