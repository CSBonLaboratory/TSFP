module Evaluation.Substitution where

import Syntax.Expression
import Data.Set

-- data Expression = Binding {symbol :: String, value :: Expression}
--     | Application {func :: Expression, param :: Expression}
--     | Lambda {formal :: String, body :: Expression}
--     | Symbol String
--     deriving Show


{-|
    Returns the list of free variables in an expression.
-}
freeVars :: Expression -> [String]
freeVars (Symbol s) = [s]
freeVars (Lambda x lb) = toList $ difference (fromList $ freeVars lb) (insert x empty)
freeVars (Application f p) = toList $ union (fromList $ freeVars f) (fromList $ freeVars p)

{-|
    Performs the substitution of the free occurrences of a variable within
    an expression with another expression.
-}
subst :: String      -- ^ Variable
      -> Expression  -- ^ New expression
      -> Expression  -- ^ Existing expression
      -> Expression  -- ^ Resulting expression
subst x e' sym@(Symbol s)
    | x == s = e'
    | otherwise = sym

subst x e' lb@(Lambda y e)
    | y == x = lb
    | y /= x && (not $ y `elem` (freeVars e')) = (Lambda y (subst x e' e))
    | otherwise = let z             = findNewSym (y ++ "#") (freeVars (Application e' e))
                      aphaConverted = subst y (Symbol z) e
                  in (Lambda z (subst x e' aphaConverted))

subst x e (Application e' e'') = (Application (subst x e e') (subst x e e''))

findNewSym :: String -> [String] -> String
findNewSym x xs
    | not $ x `elem` xs = x
    | otherwise = findNewSym (x ++ "#") xs
