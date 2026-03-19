main :: IO ()
main = do
    putStrLn "Homework 1!"
    -- print (quicksort [3, 6, 8, 10, 1, 2, 1])
    print("Ex 1. Goldbach pairs for n=28: " ++ show (goldbachPairs 28))
    print("Ex 2. Coprime pairs for [1..10]: " ++ show (coprimePairs [1..10]))
    print("Ex 3. Primes up to 20: " ++ show (primesTo 20))
    print("Ex 4. Matrix multiplication of [[1, 2], [3, 4]] and [[5, 6], [7, 8]]: " ++ show (matMul [[1, 2], [3, 4]] [[5, 6], [7, 8]]))
    print("Ex 5. Permutations of 2 elements from [1, 2, 3]: " ++ show (permutations 2 [1, 2, 3]))
-- testing
-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs) = quicksort le ++ [x] ++ quicksort gr
--     where
--         le = filter (< x) xs
--         gr = filter (>= x) xs

-- 3. Sieve of Earatosthenes
primesTo :: Int -> [Int]
primesTo n = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

isPrime :: Int -> Bool
isPrime x = x `elem` primesTo x

-- 1. Goldbach Pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
    | n < 4 = []
    | otherwise = [(p, q) | p <- [2..n], q <- [p..n], p + q == n, isPrime p, isPrime q]

-- 2. Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = [(x, y) | x <- xs, y <- xs, x < y, gcd x y == 1]

-- 4. Matrix multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul firstList secondList
    | null firstList || null secondList || null (head secondList) = []
    | otherwise = [ 
        [sum [firstList !! i !! k * secondList !! k !! j | k <- [0..p-1]]
                | j <- [0..n-1] ]
                    | i <- [0..m-1]
        ]
    where
        m = length firstList 
        p = length (head firstList)
        n = length (head secondList)

-- 5. Permutations
permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k lst = [x:ps | (i,x) <- zip [0..] lst, ps <- permutations (k-1) (take i lst ++ drop (i+1) lst)]