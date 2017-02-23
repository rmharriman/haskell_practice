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
