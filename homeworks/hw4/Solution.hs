-- 1. Functor, Applicative and Monad instances
newtype Reader r a = Reader { runReader :: r -> a}
-- The Reader monad represents computations that can read values from a shared environment. 
-- It is essentially a wrapper around a function r -> a, where r is the (read-only) environment threaded implicitly through the computation.

-- a) implement the three standard instances for Reader r:
-- it allows to use functions on a values wrapped in the Reader monad
instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader g) = Reader $ \r -> f (g r)

-- it allows us to grab a function wrapped in the Reader monad and apply it to a value wrapped in the Reader monad
instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure x = Reader $ \_ -> x

    liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
    liftA2 f (Reader g) (Reader h) = Reader $ \r -> f (g r) (h r)

-- it allows us to chain computations that depend on the same environment
instance Monad (Reader r) where
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader g) >>= f = Reader $ \r -> runReader (f (g r)) r

-- The intended semantics are the usual ones: fmap f r runs r in the environment and then applies f to the result; 
-- pure x ignores the environment and returns x; liftA2 f ra rb runs both ra and rb in the same environment and combines their results with the binary function f; 
-- (>>=) sequences two Reader computations, passing the same environment to both and letting the second depend on the value produced by the first.

-- 2. Primivite operations
-- Implement the basic Reader primitivies - these are the oinly "public" interface you should need to write the rest of the code; once they are
-- in place, prefer them (and do-notation) over pattern-matching on the Reader constructor directly.
-- Retrieves the entire enviornment
ask :: Reader r r
ask = Reader $ \r -> r

-- Retrieves a value derived from the environment by applying a projection,
-- e.g. 'asks interestRate :: Reader BankConfig Double`.
asks :: (r -> a) -> Reader r a
asks f = Reader $ \r -> f r

-- -- Runs a subcomputation in a locally modified environment. The modification
-- is only visible inside the passed Reader — once it returns, the outer
-- environment is restored (conceptually; there is no mutable state, the
-- modified environment simply goes out of scope).
local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader $ \r -> g (f r)

-- 3. A practical example - banking system
-- Consider a small banking system where the bank's configuration (interest rate, fees, limits) is the read-only environment shared by every operation:
data BankConfig = BankConfig
    { interestRate :: Double -- annual interest rate (e.g. 0.05 for 5%)
    , transactionFee :: Int -- flat fee charged per transaction
    , minimumBalance :: Int -- minimum required balance on an account
    } deriving (Show)

data Account = Account
  { accountId :: String       -- account identifier
  , balance   :: Int          -- current balance
  } deriving (Show)

-- Implement the following four functions using the Reader monad. 
-- Prefer ask / asks and do-notation over pattern-matching on the Reader constructor — this is what makes the monadic style pay off:

-- Computes the interest accrued on the account, based on the configured rate.
-- The result should be an Int — round or truncate as you see fit, but be consistent.
calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
    config <- ask
    let rate = interestRate config
    return $ floor (fromIntegral (balance account) * rate)

-- Deducts the transaction fee from the account and returns the updated account.
-- The accountId should remain unchanged.
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
    config <- ask
    let fee = transactionFee config
    return account { balance = balance account - fee }

-- Checks whether the account balance meets the configured minimum.
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
    config <- ask
    let minBalance = minimumBalance config
    return $ balance account >= minBalance

-- Runs the three operations above on a single account and combines their results.
-- The returned tuple contains:
-- * the account after the transaction fee has been applied,
-- * the interest computed from the ORIGINAL account,
-- * whether the ORIGINAL account meets the minimum balance requirement.
processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
    interest <- calculateInterest account
    updatedAccount <- applyTransactionFee account
    meetsMinimum <- checkMinimumBalance account
    return (updatedAccount, interest, meetsMinimum)

main :: IO ()
main = do
    putStrLn "Homework 4!"
    print ("Ex 3: ")
    print "Calculate interest on account balance = 1000 with interest rate = 0.05: "
    print (calculateInterest (Account { accountId = "123", balance = 1000 }) `runReader` BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }  ) -- should print 50
    print "Account balance = 1000 after applying transaction fee = 2: "
    print (applyTransactionFee (Account { accountId = "123", balance = 1000 }) `runReader` BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 } ) -- should print Account { accountId = "123", balance = 998 }
    print "Account balance = 1000 meets minimum balance = 100: "
    print (checkMinimumBalance (Account { accountId = "123", balance = 1000 }) `runReader` BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 } ) -- should print True
    print "Account balance = 50 meets minimum balance = 100: "
    print (checkMinimumBalance (Account { accountId = "123", balance = 50 }) `runReader` BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 } ) -- should print False
    print "Process account with balance = 1000 with interest rate = 0.05, transaction fee = 2, minimum balance = 100: "
    print (processAccount (Account { accountId = "123", balance = 1000 }) `runReader` BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 } ) -- should print (Account { accountId = "123", balance = 998 }, 50, True)
