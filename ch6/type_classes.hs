

-- ch6 covers typeclasses in detail - one of the most powerful features of the language

-- typeclasses define generic interfaces that provide a common feature set over a variety of types

-- example of a string equality operator
stringEq :: [Char] -> [Char] -> Bool
-- two empty strings are equal
stringEq [] [] = True
-- check the first letter of the strings and recursively check the rest returning a chained boolean
-- if even one letter is off, this would return False
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
-- Everything else doesnt match
stringEq _ _ = False

-- need for typeclasses:
-- we'd have to write a new function to check int's and any other type we creaet
-- This is a pain and inefficient
-- the equality operator should be able to accept any type (be generic)

-- typeclasses solve this problem

-- typeclasses define a set of functions that can have different implementations depending on the type of the data
-- they appear to be like OO objects but they are truly different

-- First define the typeclass itself
class BasicEq a where
    isEqual :: a -> a -> Bool

-- read as declare a class (not like OO class!!) named BasicEq with instance types referred to as "a"
-- An instance type of this typeclass is any type that implements the functions defined in the typeclass.
-- in this case only one function "isEqual"

-- *Main> :t isEqual
-- isEqual :: BasicEq a => a -> a -> Bool
-- Read as:
-- For all types a, so long as a is an instance of Basic Eq, isEqual takes two parameters of type a and returns a Bool

-- Now define isEqual for a particular type (Bool)
instance BasicEq Bool where
    isEqual True True   = True
    isEqual False False = True
    isEqual _     _     = False

-- example of a typeclass definition with 2 functions:
class BasicEq2 a where
    isEqual2 :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool

-- Can also provide default implementations too
class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)

-- anyone implementing this class needs to implement one or the other function
-- while it looks like the default would work w/o implementing either, each function depends on the other
-- endless loop would result if we did not implement at least one of these functions


-- How to define instances of typeclasses, i.e., implement all functions of that typeclass

-- example of a basic equality test not using typeclasses
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = False

-- Now make Color a member (instance) of the BasicEq3 typeclass instead
-- No need to repeat the type signature as it's the same as the default implementation
-- We also get isNotEqual3 working for free with the default implementation and the isEqual3 custom implementation
instance BasicEq3 Color where
    isEqual3 Red Red = True
    isEqual3 Green Green = True
    isEqual3 Blue Blue = True
    isEqual3 _ _ = False

-- Haskell comes with many built-in typeclasses

-- Show - converts values to strings - defined for many types
-- custom types can be made to be instances of the typeclass to make it easy to display or print them
-- Show typeclass defines the show function: show :: (Show a) => a -> String
-- make our color data type a member of the show typeclass
-- also read as define a Show instance
instance Show Color where
    show Blue = "Blue"
    show Red = "Red"
    show Green = "Green"

-- Show is usually used to define a String representation for data that is useful for a machine to parse back with Read.

-- Read - essentially the opposite of Show
-- defines functions that will take a String, parse it, and return data in any type that is a member of Read
-- read is the most useful function of the Read typeclass: read :: (Read a) => String -> a
-- read typically takes an explicit type to parse the input
-- read returns a value of type Read a => a
-- show expects a value of type Show a => a
-- there are many types that define instances of both
-- w/o an explicit type, the compiler must guess which parser to use

-- example of trying to read in a string, but the compiler doesnt know how to parse it (int or double)
--ghci> read "5"
-- <interactive>:1:0:
--    Ambiguous type variable `a' in the constraint:
--      `Read a' arising from a use of `read' at <interactive>:1:0-7
--    Probable fix: add a type signature that fixes these type variable(s)

-- Much better:
-- ghci> (read "5")::Integer
-- 5
-- ghci> (read "5")::Double
-- 5.0
-- ghci> (read "5.0")::Integer
-- *** Exception: Prelude.read: no parse




