-- file: ch05/simplejson
module SimpleJSON (
    JValue(..),
    getString,
    getInt,
    getDouble,
    getBool,
    getObject,
    getArray,
    isNull
    ) where

-- Haskell source file defines a single module. the module lets us determine what is accessible outside the module
-- also called a module definition and must precede all other definitions

-- Rules:
-- module is a reserved word in haskell
-- module names must be capitalized
-- module name must be the same as the prefix of the filename
-- following module name is a list of exports to indicate what is visible
-- where keyword indicates the body of the definition follows
-- (..) after the JValue name indicates both the type constructor and all value constructors are exported
-- omitting the exports from a module declaration exports everything (what we've done til now)
        -- module ExportEverything where

-- Compiling Haskell Source - ghc compiles haskell to native code
    -- example command: ghc -c SimpleJSON.hs
    -- -c tells compiler to only generate object code
    -- if the -c were omitted, compiler would attempt to create an executable (fails w/o a main function)

    -- .hi is an interface file - contains info about the exported names
    -- .o is the object file with machine generated code




-- JSON has 4 basic types: string, numbers, booleans, and null
-- JSON has 2 compound types: ordered sequence of values (array), unordered key/value pairs (object - python dict)
        -- key is always a string; value can be any type

-- to work with JSON, we first build an algabraic data type for it (need to represent all possible types)
-- for each type we create a value constructor (most have parameters)
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

-- Constructors are used to create JValues, pattern matching is used to go in the other direction
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

-- more accessor functions:
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull

-- truncate function turns a floating point or ration number into an integer by dropping digits after decimal point

