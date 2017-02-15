-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)

myInfo = Book 978013507 "Alegebra of Programming" ["Richard Bird", "Oege de Moor"]


-- type synonyms
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

-- Example of multiple value constructors (VC) - everything after value constructor are arguments to the VC
data BillingInfo = CreditCard CardNumber CardHolder Address
                   | CashOnDelivery
                   | Invoice CustomerID
                   deriving (Show)

-- Better way to define a datatype. Accessor functions are provided for you.
-- Haskell calls it record syntax
data Customer = Customer {
      customerID      :: CustomerID,
      customerName    :: String,
      customerAddress :: Address
    } deriving (Show)

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