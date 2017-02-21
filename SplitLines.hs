-- Type signature for new function. Accepts a string (contents of a file), and returns a list of strings (lines)
-- break is a standard library function - takes 2 args a function and a list
--   the function must examine an element of the list and returns a Bool to indicate whether to break or not
--   break returns a pair (tuple with two values)
--      first is sublist consumed before predicate returned
--      second is the rest of the list
splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
     let (pre, suf) = break isLineTerminator cs
     in pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []
isLineTerminator c = c == '\r' || c == '\n'
