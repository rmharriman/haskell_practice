-- ch4 lambda and partial functions

import Data.List (isInfixOf)
import Data.Char (isSpace)


-- example function that takes a sequence and checks if the sub-sequence is contained in it
-- i.e., is the sub-sequence needle anywhere in haystack
-- this example from the book does not seem to work. sounds good in theory tho
-- isInAny needle haystack = any inSequence haystack
   -- where inSequence s = needle `isInfixOf` s

-- can rewrite the function above using anonymous inner functions (lambda)
-- in haskell, lambda's are written as a "\" (backslash) as this sort of looks like greek letter lambda
-- isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack

-- both are horribly broken, but they show the same thing and demo lambda
-- can use pattern matching and guards in definitions
-- there are restrictions on lambda tho: Can only have a single clause. (i.e., only one pattern)
-- lambda does help with conciseness but readability does take a hit

-- read as myDropWhile takes a function as it's first arg and returns a function that takes one arg
-- myDropWhile :: (a -> Bool) -> [a] -> [a]

-- :type dropWhile isSpace
-- dropWhile isSpace :: [Char] -> [Char]
-- essentially, this is a function that takes a list of Chars as the arg
-- powerful, yet subtle, feature - now dropWhile isSpace can be used as an arg to higher order functions e.g. map

-- Normally zip3 takes one arg, but when we apply it with one arg, we get a function that accepts two args
-- Prelude Data.Char> :type zip3 "foo"
-- zip3 "foo" :: [b] -> [c] -> [(Char, b, c)]

-- All haskell functions take one arg
-- When we give fewer args to a function than it expects, we call that partial application of the function
-- We're applying the function to some of it's arguments
-- e.g., zip3 "foo"

-- Partial functions allow us to avoid writing tiresome throwaway functions and often more useful than lambda
-- 3 ways of the same thing (all horribly broke):
-- named helper
isInAny needle haystack = any inSequence haystack
    where inSequence s = needle `isInfixOf` s

-- lambda
isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack

-- partial function application
isInAny3 needle haystack = any (isInfixOf needle) haystack
-- the expression isInfixOf needle is the partially applied function (named currying after Haskell Curry)

-- currying is a very powerful and common technique in haskell
-- take our old friend niceSum
niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs

-- can be re-written to use currying
nicerSum :: [Integer] -> Integer
nicerSum = foldl (+) 0

-- partially applied infix functions are called sections
-- notation is enclosing the function with one arg in parens
x = (1+)
y = x 2

-- *Main> map x [1,2,3,4]
-- [2,3,4,5]


-- can fix second arg by using infix notation
e = (`elem` ['a'..'z'])

-- *Main> e 'd'
-- True
-- *Main> e '1'
-- False
