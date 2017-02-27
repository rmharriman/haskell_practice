-- exercises for ch 4 on folding

import Data.Char (digitToInt, isDigit)

asInt_fold :: String -> Int
asInt_fold ""                 = 0
asInt_fold (x:xs) | x == '-'  =  (-1) * asInt_fold xs
                  | x == '+'  = asInt_fold xs
                  | otherwise = foldl charAsInt 0 (x:xs)
     where charAsInt :: Int -> Char -> Int
           charAsInt acc x = acc * 10 + digitToInt x

asInt_fold_error :: String -> Int
asInt_fold_error []       = error "Contains no digits"
asInt_fold_error ('-':xs) = -(asInt_fold_error xs)
asInt_fold_error xs       = foldl charAsInt 0 xs
  where charAsInt :: Int -> Char -> Int
        charAsInt acc x | isDigit x = acc * 10 + digitToInt x
                        | otherwise = error ("Not a digit '" ++ [x] ++ "'")


myConcat :: [[a]] -> [a]
myConcat [] = error "Contains no contents"
myConcat xs = foldr myAdd [] xs
    where  myAdd :: [a] -> [a] -> [a]
           myAdd acc ys = foldr (:) acc ys

myConcat_foldr :: [[a]] -> [a]
myConcat_foldr xs = foldr (++) [] xs

myEven :: Int -> Bool
myEven x | x `mod` 2 == 0 = True
         | otherwise = False


-- Exercise was to write the builtin any function using foldr or foldl
-- clever solution uses the or function (||) and map
-- first, map turns the list of one type into a list of bool's using the supplied predicate function
-- then foldr runs the or function on the new list an returns the accumulator
-- in this case the accumulator is a Bool and if one True is encountered, it will return true
myAny :: (a -> Bool) -> [a] -> Bool
myAny pred xs = foldr (||) False (map pred xs)



