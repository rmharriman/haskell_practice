-- ch04 loop practice

-- Haskell does not have for or while loops but can use recursion to accomplish the same thing


import Data.Char (digitToInt)

asInt :: String -> Int
loop :: Int -> String -> Int

asInt xs = loop 0 xs

-- Handling the empty and non-empty cases separately, is a kind of approach called structural recursion
-- loop is also an example of tail recursion. calling yourself as the last step in a function
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs
loop acc [] = acc


-- another basic looping example
square :: [Double] -> [Double]
square (x:xs) = x*x : square xs
square [] = []

-- recursive functions do one piece of work per element
-- such a common pattern, map was created as part of the standard library to do exactly that
-- square rewritten to use map
square2 xs = map squareOne xs
    where squareOne x = x * x

-- :type map
-- map is a higher order function
-- map takes two args:
-- first is a function that takes a value of type a and returns one of type b
-- first arg in type sig (a -> b), second arg in type sig [a]
-- map :: (a -> b) -> [a] -> [b]
-- myMap shows how to implement map
myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _ = []