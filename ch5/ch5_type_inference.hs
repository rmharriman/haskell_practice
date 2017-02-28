-- ch 5 - review of type inference

import Data.Char (toUpper)

upcaseFirst (c:cs) = toUpper c

-- forgot to add :cs to the return value
-- you think you're returning a String, but actually it's a char
-- explicit type declaration would've caught this

-- best for new haskell programmers to be aggressive with type signatures - always for top level declarations
-- helps with readability and maintainability (you can remember what is going on easier)

