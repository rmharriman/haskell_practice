-- ch4 as-patterns
import Data.List (tails)
import Data.Char (isUpper)

-- tails function is a generalised tail function: instead of returning one tail, it returns all of them
ex = tails "foobar"
-- ["foobar","oobar","obar","bar","ar","r",""]


-- tails function can be re-written to exclude non-empty suffixes (exclude the last "")
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

-- "xs@(_:xs')" is called an as-pattern - "bind the variable xs to the value that matches the right hand side of the @"
-- if the pattern is a match, xs will be bound to the entire pattern
-- which means xs' is bound to the the list xs except for the head

-- can do the same thing without an as-pattern, but the code is less readable
noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern _ = []

-- as patterns: more readable and also can save memory
-- in noAsPattern, when we match (x:xs), we construct a new copy of it
-- in suffixes, the xs value is reused and avoids allocation of a new copy

-- suffixes is nice, but there is already a better way to do what we need
suffixes2 xs = init (tails xs)

-- init returns all but the last element of a list

-- taking a step back, all suffixes2 is doing is applying a function, then applying another function to the result
-- this can be turned into a function definition
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- can rewrite the improved suffixes function to use our compose function
suffixes3 xs = compose init tails xs
-- then improve it with automatic currying
suffixes4 = compose init tails

-- our compose function is actually builtin to the standard library as the (.) operator
-- called function composition
suffixes5 = tails . init

-- new functions can be created by chaining together functions using the composition operator
capCount = length . filter (isUpper . head) . words

-- examine the pieces of the chained function remembering the (.) operator is right associative so we proceed R to L
-- Words is a function that takes a string and returns a list of Strings (breaks a string by empty space)

-- (isUpper . head): function directly to the left of words must accept a list of strings
-- accepts a list of chars and returns a Bool

-- *Main> (isUpper . head) "hda"
-- False
-- *Main> (isUpper . head) "Hda"
-- True

-- filter takes a function and a list and runs the function on every element of the list
-- the function must return a Bool to determine if the value is added to the second list

-- List manipulation operations most often expressed using combinations of library functions: map, take, and filter
-- Middleware is fold functions: connect list functions and tail recursion
-- Try to use library functions first, second a fold, and finally hand roll your own tail recursion
-- if there is a fourth, it would be anonymous functions
-- reminder tail recursiion: last thing that loop does is simply call itself (very general)




