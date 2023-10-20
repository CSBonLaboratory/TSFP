module Syntax.Expression where

import Syntax.Parser
import Prelude
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as M

data Expression = Binding {symbol :: String, value :: Expression}
    | Application {func :: Expression, param :: Expression}
    | Lambda {formal :: String, body :: Expression}
    | Symbol String
    deriving (Show, Eq)

-- Lab 3
type Context = M.Map String Expression

-- Lab 6
-- type Eval = State Context

-- Lab 8
type Eval a = StateT Context (ExceptT String IO) a

-- type Eval a = StateT Context (ExceptT String IO) a

-- instance Show Expression where
--     show e = showLevel 0 e

-- instance (Show Expression) => (Show [Expression]) where
-- show exprs = foldl (\acc e -> acc ++ (Prelude.show e) ++ "\n") ""

-- showLevel :: Int -> Expression -> String
-- showLevel lvl (Binding sym val) = (replicate lvl '\t') ++ "Binding" ++ "\n" ++ (Prelude.show sym) ++ "\n" ++ (showLevel (lvl + 1) val)
-- showLevel lvl (Symbol s) = (replicate lvl '\t') ++ "Symbol" ++ (Prelude.show s) ++ "\n"
-- showLevel lvl (Application f p) = (replicate lvl '\t') ++ "Application" ++ "\n" ++ (showLevel (lvl + 1) f) ++ (showLevel (lvl + 1) p)
-- showLevel lvl (Lambda f b) = (replicate lvl '\t') ++ "Lambda" ++ "\n" ++ (Prelude.show f) ++ "\n" ++ (showLevel (lvl + 1) b)


