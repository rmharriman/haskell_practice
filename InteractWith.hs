import System.Environment (getArgs)

-- Some new keywords introdueced here:
-- do keyword represents a block of actions
-- <- is the equivalent of assignment in a do block
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
           args <- getArgs
           case args of
             [input, output] -> interactWith function input output
             _ -> putStrLn "error: exactly two arguments needed"

        myFunction = id
