-- file: ch03/BookStore.hs
-- Every expression and function in Haskell has a type -- HS type system is a MAJOR +
-- STRONG - will not coerce/cast one type to another
-- STATIC - compiler knows the type of every value and expression at compile time (as opposed to python duck typing)
-- INFERRED - compiler can auto-detect type for us (we can also optionally explicitly declare it)


-- BookInfo is type constructor (name of our new type), while Book is value constructor (must start w/ a capital)
-- Use the Book value constructor to create a value of type BookInfo
-- Components follow the value constructor (fields in other languages) - a slot to keep a value
data BookInfo = Book Int String [String]
                deriving (Show)

-- Create new values of type BookInfo by using the Book value constructor as a function
-- Book is a function that creates and returns a new value of type BookInfo
myInfo = Book 978013507 "Alegebra of Programming" ["Richard Bird", "Oege de Moor"]


-- type synonyms (purely for making code more readable)
type CustomerID = Int
type ReviewBody = String

-- type constructor and value constructor are often the same
-- type constructors are used in the type signature (declaration) and value constructors are used in the code
data BookReview = BookReview BookInfo CustomerID ReviewBody

-- type synonym - new name that refers to an existing type
type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]


-- algebraic data types group related values together to form a compound value. ex:
data Cartesian2D = Cartesian2d Double Double
                   deriving (Eq, Show)

-- Can also be an enumeration type (range of possible values)
data Roygbiv = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Eq, Show)

-- Example of multiple value constructors (VC) - everything after value constructor are arguments to the VC
data BillingInfo = CreditCard CardNumber CardHolder Address
                   | CashOnDelivery
                   | Invoice CustomerID
                   deriving (Show)

-- Underscore acts as a wild card in pattern matching (accessor function example)
-- Writing your own accessor functions can be tedious
nicerID (Book id _  _) = id
nicerTitle (Book _ title _ ) = title
nicerAuthor (Book _ _ author) = author


-- Better way to define a datatype. Accessor functions are provided for you.
-- Haskell calls it record syntax (use curly brackets)
data Customer = Customer {
      customerID      :: CustomerID,
      customerName    :: String,
      customerAddress :: Address
    } deriving (Show)

-- No need to write the accessor function: customerID (Customer id _ _) = id
-- Output of :type customerID is already what we need: customerID :: Customer -> CustomerID
-- Read as create function customerID which takes an argument of type Customer and returns a CustomerID type


-- Usual application syntax to create a value of this type
customer1 = Customer 1005 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]

customer2 = Customer 45454 "Rob" ["where i live", "USA"]

-- Record syntax can also be used to create records too
-- in this way, order can be changed (no longer positional like the previous way
customer3 = Customer {
              customerID = 271828,
              customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"],
              customerName = "Jane Q. Citizen"
            }

-- Parameterized Types
-- Type Variables are used to introduce polymorphism to our types
data Maybe a = Just a
             | Nothing
             deriving (Show)
-- a is a type variable - indicates the Maybe type takes another type as a parameter
-- Maybe can be used on any type as a result (it is generic/polymorphic)

-- Recursive Types - classic example is list type (defined in terms of itself)
-- Make our own custom list swapping : for Cons and [] for Nil
data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- Code Output:
-- *Main> Cons 0 Nil
-- Cons 0 Nil
-- *Main> Cons 1 it
-- Cons 1 (Cons 0 Nil)
-- *Main> Cons 2 it
-- Cons 2 (Cons 1 (Cons 0 Nil))
-- *Main> Cons 3 it
-- Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil)))
