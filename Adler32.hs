-- ch04 Adler32.hs
-- more recursion practice
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc


-- new concepts:
-- shiftL implements a logical shift left
-- .&. provides a bitwise and
-- .|. provides a bitwise or
-- 0xff is the hexadecimal number FF which has a integer value of 255
-- binary reprsentation - 00000000000000000000000011111111


base = 65521

adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b _      = (b `shiftL` 16) .|. a


-- Line by line in prose:
-- adler32 xs --- define a function called adler32 that takes one argument, a list
-- helper 1 0 xs ---- that our new function calls a locally defined function with 3 args, 1 0 and the list
-- where helper a b (x:xs) --- define a local function helper which takes 3 args - one of type a, type b and a list
--                             pattern matches for a non-empty list and a wildcard


-- can be re-written to use a pair (tuple with two items) instead of two args
adler32_try2 xs = helper (1,0) xs
    where helper (a,b) (x:xs) =
               let a' = (a + (ord x .&. 0xff)) `mod` base
                   b' = (a' + b) `mod` base
               in helper (a',b') xs
          helper (a,b) _      = (b `shiftL` 16) .|. a

-- this is a little better as now it extracts common behavior into a higher order function
--   "do something to every element of a list, updating an accumulator as we go, return the accumulator at the end"
-- This kind of function is called a fold, because it "folds up" a list.
--      there are two kinds of folding: foldl (folding from the left or start) and foldr (from the right or end)


-- more generic foldl
-- type sig: func foldl takes 3 args, a function (a step), initial value of type a, and a list and returns a type a
-- function arg takes two args, one of type a and one of type b and returns a type a
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl step zero (x:xs) = Main.foldl step (step zero x) xs
foldl _    zero []     = zero

-- the earlier mySum function can be rewritten to use foldl
foldlSum xs = Main.foldl step 0 xs
    where step acc x = acc + x

-- And again as the step function is just addition in this case
niceSum :: [Integer] -> Integer
niceSum xs = Main.foldl (+) 0 xs

-- the Adler32 function can be rewritten to see why using a pair was important
-- bc the step function takes a pair, it will return one as well.
adler32_foldl xs = let (a,b) = Main.foldl step (1,0) xs
                   in (b `shiftL` 16) .|. a
   where step (a, b) x = let a' = a + (ord x .&. 0xff)
                         in (a' `mod` base, (a' + b) `mod` base)

-- folding can start from the right as well.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr step zero (x:xs) = step x (Main.foldr step zero xs)
foldr _    zero []     = zero


-- great illustration of foldr.
-- replace the empty list with the zero value (in this case, actually 0)
-- then replace the list contructor with an application of the step function
-- 1 : (2 : (3 : []))
-- 1 + (2 + (3 + 0 ))

-- foldr seems less useful but filter can be rewritten to use it
-- foldr step [] xs is not calling step! this is the initial call to foldr only
myFilter p xs = Main.foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys

-- myFilter takes a function and a list as args
-- then runs a locally defined function "step" for each element in the list starting with the right end
-- the local step function in turn calls the func supplied to myFilter on each element
-- if the call evaluates to true, the element is added to the accumulator (in this case an empty list)

-- foldr's class of functions is called primitive recursion (map is also implemented using foldr)
-- many list manipulation functions are in the primitive recursion class
-- map written in temrs of foldr:
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = Main.foldr step [] xs
    where step x ys = f x : ys

-- foldr transforms a list.
-- first arg tell it what to do with each head/tail element of the list
-- second arg tells it what to substitute for the end of the list (what we've called the accumulator)
-- Append function  highlights this. step function is the list contructor and the accumulator is the list to append
myAppend :: [a] -> [a] -> [a]
myAppend xs ys = Main.foldr (:) ys xs

-- suffice to say, foldr is a very important tool in Haskell

-- Extreme challenge of the day: Understand foldl implemented using foldr
-- myFoldl defines a function that takes 3 args
-- arg 1 is a function that takes 2 args, arg 2 is of type a, arg 3 is a list of type b
-- myFoldl returns a single value of type a (same as arg 2)
myFoldl :: (a -> b -> a) -> a -> [b] -> a

-- myFoldl calls foldr with the local step function and 3 more args
-- seems like too many args to the foldr function?
myFoldl f z xs = Main.foldr step id xs z
    where step x g a = g (f a x)

-- pitfalls of foldl
-- final evaluation will not be computed until it's requested (Haskell's lazy evaluation)
-- before it is evaluated, it will be stored as a thunk, which is more expensive than a single number
-- the more complex the thunked expression, the more space it needs
-- large foldl computations run the risk of space leaks (expanding to use more memory than it should)
-- GHC places a cap on thunk space so an error is thrown before all memory can be consumed



