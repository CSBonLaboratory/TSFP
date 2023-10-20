module Reader where

{-
    The Reader monad is similar to the State monad, but the hidden state
    is seen only as an initial read-only environment, which is used by all
    sequenced computations, but is never "changed".

    Defined in Control.Monad.Reader.
    See https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html

    A possible implementation is given below, where r is the type
    of the environment, and a is the type of the result of the computation.
    By analogy with the state monad, the second s in s -> (a, s) would no longer
    be necessary, since the first s is never "modified".

    TODO: Replace all the undefined portions below, such that
    (runReader test 10) returns 225.
-}
newtype Reader r a = Reader { runReader :: r -> a }

instance Monad (Reader r) where
    return a = Reader $ \r -> a

    m >>= f =  Reader $ \r -> let a = runReader m r in runReader (f a) r

{-
    Returns the environment as the result.
-}
ask :: Reader r r
ask = Reader $ \r -> r

{-
    Runs the given computation within a modified environment, obtained
    by transforming the existing environment through the given function.
    The transformation is only visibile within the "local" computation.

    Examples:

    >>> runReader (local (* 10) double) 10
    200
-}
local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \r -> let r' = f r in runReader m r'

{-
    >>> runReader test 10
    225
-}
test :: Reader Int Int
test = do
    x <- double
    y <- local (* 10) double  -- multiplies the environment by 10 before doubling
    z <- half                 -- the previous transformation has no effect here
    return $ x + y + z

{-
    Returns two times the environment, which is an Int.

    Examples:

    >>> runReader double 10
    20
-}
double :: Reader Int Int
double = do
    x <- ask
    return $ x * 2

{-
    Returns half the environment, which is an Int.

    Examples:

    >>> runReader half 10
    5
-}
half :: Reader Int Int
half = do
    x <- ask
    return $ x `div` 2