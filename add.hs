-- file: ch03/add.hs
add a b = a + b

-- Example of pattern matching
-- Define the behavior of the function myNot based on the input pattern. (function is defined as a series of equations)
-- checks patterns for matches in the order in which we specify them in our equations
myNot True = False
myNot False = True