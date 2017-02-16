-- file: ch03/add.hs
add a b = a + b

-- Example of pattern matching
-- As defined as look inside a value and bind variables to the data it contains.
-- Define the behavior of the function myNot based on the input pattern. (function is defined as a series of equations)
-- checks patterns for matches in the order in which we specify them in our equations
myNot True = False
myNot False = True

-- Better example of a function with different patterns
-- Sums a list while there are contents, but returns 0 when the list is empty
sumList (x:xs) = x + sumList xs
sumList []     = 0

-- Evaluation of sumList [1,2]:
-- list notation [1,2] is shorthand for the expression (1:(2:[]))
-- Begin by trying to match with the first pattern, (x:xs) (: is the list constructor but can be used to pattern match)
-- Value (1:(2:[])) was constructed with (:) so the value matches (succeeds)
-- Upon success, variables x and xs are bound to the constructors arguments (x is given value 1, xs 2:[])
-- 1 + sumList 2:[] is evaluated next. it also succeeds with (x:xs) - x bound to 2 and xs to []
-- Finally, evaluating 1 + 2 + sumList [] fails at first pattern and falls thru to the second, which returns 0
-- Full result: 1 + (2 + (0)) or 3


