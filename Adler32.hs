-- ch04 Adler32.hs
-- more recursion practice
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

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

foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _    zero []     = zero


-- the earlier mySum function can be rewritten to use foldl
foldlSum xs = foldl step 0 xs
    where step acc x = acc + x

