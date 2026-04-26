import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad (foldM, guard)
import Data.List (delete)
import Control.Monad.Writer

-- 1. Maze navigation
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

-- a) Function that attempts a single move in the given direction, returning Nothing if blocked by a wall
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    neighbors <- M.lookup pos maze
    M.lookup dir neighbors

-- b) Function that follows a sequence of directions from a starting position, short-circuiting to Nothing as soon
-- as any step is blocked. Use the Maybe monad - do not pattern-match on Nothing manually
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze start dirs = foldM (move maze) start dirs

-- c) Function that returns the full trace of positions visited (including the start), or Nothing if the path is blocked at any point
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = return [pos]
safePath maze pos (d:ds) = do
    nextPos <- move maze pos d
    rest <- safePath maze nextPos ds
    return (pos : rest)


-- 2. Decoding a message
type Key = Map Char Char

-- a) Function that decodes the entire string, returning Nothing if any character in the input is missing from the key. Use traverse with the Maybe monad.
decrypt :: Key -> String -> Maybe String
decrypt key = traverse (\c -> M.lookup c key)

-- b) Function that decrypts a list of words, failing if any single word cannot be fully decoded
decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)


-- 3. Settling arrangments
-- You are organizing a dinner and must assign guests to seats around a table. Some pairs of guests have conflicts and must not sit next to each other.
type Guest = String
type Conflict = (Guest, Guest)

-- a) Function that returns all valid permutations of the guest list such that no two conflicting guests are adjacent (the table is round, so the last
-- and first guests are also neighbours). Use the list monad to generate permutations amd guard to filter out invalid ones.

isConflicted :: [Conflict] -> Guest -> Guest -> Bool
-- check both directions since seating conflicts are mutual
isConflicted conflicts g1 g2 = (g1, g2) `elem` conflicts || (g2, g1) `elem` conflicts

-- helper checking if an entire seating arrangement is valid
isValid :: [Conflict] -> [Guest] -> Bool
isValid _ [] = True
isValid _ [_] = True
isValid conflicts arrangment =
    let
        -- pair each guest with the one sitting next to them
        -- appending [head arrangment] to the end to account for the round table
        pairs = zip arrangment (tail arrangment ++ [head arrangment])
    in
        not $ any (\(g1, g2) -> isConflicted conflicts g1 g2) pairs

perms :: Eq a => [a] -> [[a]]
perms [] = return []
perms xs = do
    x <- xs
    let rest = delete x xs
    xs' <- perms rest
    return (x : xs')

-- Generates all permutations and uses guard to eliminate invalid branches
settings :: [Guest] -> [Conflict] -> [[Guest]]
settings guests conflicts = do
    arrangment <- perms guests
    guard (isValid conflicts arrangment) -- kill the branch if invalid (guard)
    return arrangment -- keep if passed the guard


-- 4. Result monad with warnings
data Result a = Failure String | Success a [String] deriving (Show) -- deriving (Show) for print test purposes
-- Failure msg represents a computation that failed with an error message, and Success val warnings represents a successful computation
-- carrying a value together with a list of accumulated warning messages.

-- a) Implement Functor, Applicative, and Monad instances for Result. Warnings should be accumulated (concatenated) when sequencing computations.

instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success val warnings) = Success (f val) warnings

instance Applicative Result where
    -- `pure` injects a value with no warnings
    pure val = Success val []

    -- if the first computation fails, propagate the error
    Failure msg <*> _ = Failure msg
    -- If the first succeeded but the second failed, propagate the second's error
    Success _ _ <*> Failure msg = Failure msg
    -- If both succeed, apply the function and concatenate warnings
    Success f w1 <*> Success x w2 = Success (f x) (w1 ++ w2)

instance Monad Result where
    -- if starts with a failure, short-circuit and skip the next function
    Failure msg >>= _ = Failure msg

    -- if we start with a success, we pass the value to the next function
    Success val w1 >>= f = case f val of
        -- if the next function fails, the whole chain fails
        Failure msg -> Failure msg
        -- If the next function succeeds, we concatenate the historical 
        -- warnings (w1) with the new warnings (w2)
        Success newVal w2 -> Success newVal (w1 ++ w2)

-- b) Write helper functions:
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure
-- c) Use the Result monad to implement a function
validateAge :: Int -> Result Int
validateAge age
    | age < 0 = failure "Age cannot be negative"
    | age > 150 = do
        warn $ "Warning: Age " ++ show age ++ " is too high"
        return age
    | otherwise = return age

-- that fails if the age is negative, warns if the age is above 150, and otherwise succeeds with the age. Then implement:
validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge
-- that validates a list of ages, accumulating all warnings. Use mapM.


-- 5. Evaluator with simplification log
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show) -- deriving Show for testing purposes
-- write a function 
simplify :: Expr -> Writer [String] Expr
-- that applies algebraic simplification rules and logs each rule applied. THe rules should include at least:
-- Add (Lit 0) e or Add e (Lit 0) simplifies to e (additive identity)
-- Mul (Lit 1) e or Mul e (Lit 1) simplifies to e (multiplicative identity)
-- Mul (Lit 0) _ or Mul _ (Lit 0) simplifies to Lit 0 (zero absorption)
-- Neg (Neg e) simplifies to e (double negation)
-- Add (Lit a) (Lit b) simplifies to Lit (a+b) (constant folding)
-- Mul (Lit a) (Lit b) simplifies to Lit (a*b) (constant folding)

-- Each time a rule fires, log a message like "Add identity: 0 + e -> e". Apply simplification recursively (bottom-up: simplify subtrees first, then the root).

simplify (Lit n) = return (Lit n)

simplify (Neg e) = do
    -- simplify the child first
    e' <- simplify e
    -- apply rules to newly simplified child
    case e' of
        Neg inner -> do
            tell ["Double negation: --e -> e"]
            return inner
        _ -> return (Neg e')
simplify (Add e1 e2) = do
    -- Simplify children first
    e1' <- simplify e1
    e2' <- simplify e2
    -- Apply rules to simplified children
    case (e1', e2') of
        (Lit 0, _) -> do
            tell ["Add identity: 0 + e -> e"]
            return e2'
        (_, Lit 0) -> do
            tell ["Add identity: e + 0 -> e"]
            return e1'
        (Lit a, Lit b) -> do
            tell ["Constant folding: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a+b)]
            return (Lit (a + b))
        _ -> return (Add e1' e2')
simplify (Mul e1 e2) = do
    -- Simplify children first
    e1' <- simplify e1
    e2' <- simplify e2
    -- Apply rules to simplified children
    case (e1', e2') of
        (Lit 0, _) -> do
            tell ["Zero absorption: 0 * e -> 0"]
            return (Lit 0)
        (_, Lit 0) -> do
            tell ["Zero absorption: e * 0 -> 0"]
            return (Lit 0)
        (Lit 1, _) -> do
            tell ["Multiplicative identity: 1 * e -> e"]
            return e2'
        (_, Lit 1) -> do
            tell ["Multiplicative identity: e * 1 -> e"]
            return e1'
        (Lit a, Lit b) -> do
            tell ["Constant folding: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a*b)]
            return (Lit (a * b))
        _ -> return (Mul e1' e2')


testMaze :: Maze
testMaze = M.fromList [
    ((0,0), M.fromList [(E, (1,0)), (S, (0,1))]),
    ((1,0), M.fromList [(W, (0,0)), (S, (1,1))]),
    ((0,1), M.fromList [(N, (0,0)), (E, (1,1))]),
    ((1,1), M.fromList [(N, (1,0)), (W, (0,1))])
    ]

main :: IO ()
main = do
    putStrLn "Homework 3!"
    print ("Ex 1: ")
    print ("Testing (a) move:", move testMaze (0,0) E) -- Just (1,0)
    print ("Testing (b):", followPath testMaze (0,0) [E]) -- Just (1,0)
    print ("Testing (c):", safePath testMaze (0,0) [E]) -- Just [(0,0),(1,0)]
    print ("Ex 2:" )
    let testKey = M.fromList [('a', 'x'), ('b', 'y'), ('c', 'z'), ('d', 'w'), ('e', 'n'), ('f', 'e')]
    print ("Testing (a):", decrypt testKey "abc") -- Just "xyz"
    print ("Testing (b):", decryptWords testKey ["abc", "ghi"]) -- Nothing
    print ("Testing (b) with valid words:", decryptWords testKey ["abc", "de"]) -- Just ["xyz", "wn"]
    print ("Ex 3: ")

    let guests = ["Alicia", "Bob", "Selena", "Rick"]
    let conflicts = [("Alicia", "Bob"), ("Selena", "Rick")]

    print ("Testing with guests: ", guests)
    print ("and conflicts: ", conflicts)
    print ("Valid settings: ", settings guests conflicts)

    print ("Ex 4: ")
    print ("Testing validateAge with -5: ", validateAge (-5)) -- Failure "Age cannot be negative"
    print ("Testing validateAge with 30: ", validateAge 30) -- Success 30 []
    print ("Testing validateAge with 200: ", validateAge 200) -- Success 200 ["Warning: Age 200 is too high"]
    print ("Testing validateAges with [-5, 30, 200]: ", validateAges [-5, 30, 200]) -- Failure "Age cannot be negative"
    print ("Testing validateAges with [30, 200]: ", validateAges [30, 200]) -- Success [30, 200] ["Warning: Age 200 is too high"]
    print ("Testing validateAges with [200, 300, 400, 5, 600]: ", validateAges [200, 300, 400, 5, 600]) -- Success [200, 300, 400, 5, 600] ["Warning: Age 200 is too hih", "Warning: Age 300 is too high", "Warning: Age 400 is too high", "Warning: Age 600 is too high"]

    print("Ex 5: ")

    -- expression example: (0 + 5) * 1 -> should simplify to 5 with logs
    let expr = Mul (Add (Lit 0) (Lit 5)) (Lit 1)
    putStrLn ("Original: " ++ show expr)
        
    let (result1, log1) = runWriter (simplify expr)
    
    putStrLn ("Simplified: " ++ show result1)
    putStrLn "Simplification Log:"
    mapM_ (\msg -> putStrLn (" - " ++ msg)) log1