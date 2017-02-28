-- deliberate naming of module to be called main
-- compiler expects a Main module with a function named main when creating an executable
module Main where

-- indicates we want to take all the exported names from SimpleJSON and make them available here
import SimpleJSON

-- import directives must appear after module declaration but before all other code

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])


-- now we compile our new program:
-- ghc -o simple Main.hs SimpleJSON.o
-- compiler is perfectly capable of linking (generating an executable) and compiling in one step
-- -o option tells compiler what the outputted executable file should be called
