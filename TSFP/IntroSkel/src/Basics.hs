module Basics where  -- (10p)

{-
    1. (1p)
    Implement the 'reverse' function, which returns a given list
    in reverse order, using explicit recursion.
    Do NOT employ any higher-order functions.
-}

reverseRec1 :: [a] -> [a]
reverseRec1 [] = []
reverseRec1 (x:xs) = reverseRec1 xs ++ [x]

{-
    2. (1p)
    Same as (1), but change the direction on which the output is built.
    For example, if (1) built the list on return from recursion,
    you should now built the list when recursing forward.
-}

reverseRec2 :: [a] -> [a]
reverseRec2 xs = reverseAcc xs []
    where reverseAcc [] acc = acc
          reverseAcc (x : xs) acc = reverseAcc xs (x : acc)  

{-
    3. (1.5p)
    Same as (1), but use a higher-order function instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (1).
-}

reverseHO1 :: [a] -> [a]
reverseHO1 = foldr (\x acc -> acc ++ [x]) []

{-
    4. (1.5p)
    Same as (2), but use a higher-order function instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (2).
-}

reverseHO2 :: [a] -> [a]
reverseHO2 = foldl (\acc x -> x : acc) []

{-
    5. (1p)
    Implement the power set function, which returns the set of all subsets
    of a set, using explicit recursion.
-}

powerSetRec :: [a] -> [[a]]
powerSetRec [x] = [[x], []]
powerSetRec (x : xs) = let ls = powerSetRec xs
                       in ls ++ (map (x:) ls)

{-
    6. (1.5p)
    Same as (5), but use higher-order functions instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (5).
-}

powerSetHO :: [a] -> [[a]]
powerSetHO xs = foldr(\x acc -> acc ++ (map (x:) acc)) [[]] xs

{-
    7. (0.5p)
    Compute the cartesian product of two lists, using list comprehensions.
-}

cartesian2 :: [a] -> [b] -> [(a, b)]
cartesian2 xs ys = [(x,y) | x <- xs, y <- ys]

{-
    8. (2p)
    Similar to (7), but extend to any number of lists.
-}

cartesian :: [[a]] -> [[a]]
cartesian [[]] = [[]]
cartesian [[x]] = [[x]]
cartesian xss = foldl (\yss xs -> [(x : ys) | x <- xs, ys <- yss]) start (tail $ tail xss)
    where start = [[a,b] | a <- head xss, b <- head $ tail xss]