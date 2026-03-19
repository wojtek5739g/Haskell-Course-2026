import GHC.Base (VecElem(Int16ElemRep))
main :: IO ()
main = do
    putStrLn "Homework 1!"
    -- print (quicksort [3, 6, 8, 10, 1, 2, 1])
    print("Ex 1. Goldbach pairs for n=28: " ++ show (goldbachPairs 28))
    print("Ex 2. Coprime pairs for [1..10]: " ++ show (coprimePairs [1..10]))
    print("Ex 3. Primes up to 20: " ++ show (primesTo 20))
    print("Ex 4. Matrix multiplication of [[1, 2], [3, 4]] and [[5, 6], [7, 8]]: " ++ show (matMul [[1, 2], [3, 4]] [[5, 6], [7, 8]]))
    print("Ex 5. Permutations of 2 elements from [1, 2, 3]: " ++ show (permutations 2 [1, 2, 3]))
    print("Ex 6. First 15 Hamming numbers: " ++ show (take 15 hamming))
    print("Ex 7. 3^5: " ++ show (power 3 5))
    print("Ex 8. listMaxSeq of [8, 1, 7, 12, 10, 9]: " ++ show (listMaxSeq [8, 1, 7, 12, 10, 9]))
    print("Ex 8. listMaxBang of [8, 1, 7, 12, 10, 9]: " ++ show (listMaxBang [8, 1, 7, 12, 10, 9]))
    print("Ex 9. First 10 primes: " ++ show (take 10 primes))
    print("Ex 9. Is 7883 prime? " ++ show (isPrimeInfinite 7883))
    print("Ex 10. Mean of [1.0, 2.0, 3.0, 4.0, 5.0]: " ++ show (mean [1.0, 2.0, 3.0, 4.0, 5.0]))
    print("Ex 10. MeanStrict of [1.0, 2.0, 3.0, 4.0, 5.0]: " ++ show (meanStrict [1.0, 2.0, 3.0, 4.0, 5.0]))
    print("Ex 10. Mean and Variance of [1.0, 2.0, 3.0, 4.0, 5.0]: " ++ show (meanVariance [1.0, 2.0, 3.0, 4.0, 5.0]))
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


-- 6. Hamming numbers
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs ys


hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))

-- 7. Integer Power with Bang Patterns
-- b^0=1
-- b^e = b*b^(e-1)
power :: Int -> Int -> Int
power b e = go 1 e
    where
        go :: Int -> Int -> Int
        go !acc 0 = acc
        go !acc e = go (acc*b) (e-1)

-- 8. Running Maximum

---- seq version
listMaxSeq :: [Int] -> Int
listMaxSeq(x:xs) = go x xs
    where
        go :: Int -> [Int] -> Int
        go acc [] = acc
        go acc (y:ys) =
            let !newAcc = max acc y
            in newAcc `seq` go newAcc ys

---- bang version
listMaxBang :: [Int] -> Int
listMaxBang (x:xs) = go x xs
  where
    go :: Int -> [Int] -> Int
    go !acc []     = acc
    go !acc (y:ys) = go (max acc y) ys

-- 9. Infinite Prime Stream
primes :: [Int] 

primes = sieve [2..]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

isPrimeInfinite :: Int -> Bool
isPrimeInfinite n
    | n < 2 = False
    | otherwise = null [ p | p <- takeWhile (\p -> p*p <= n) primes
                         , n `mod` p == 0 ]

-- 10. Strict Accumulation and Space Leaks

---- tail-recursive without strictness
mean :: [Double] -> Double
mean xs = go 0 0 xs
  where
    go :: Double -> Double -> [Double] -> Double
    go sum len [] = sum / len
    go sum len (y:ys) = go (sum + y) (len + 1) ys

---- tail-recursive with bang-patterns
meanStrict :: [Double] -> Double
meanStrict xs = go 0 0 xs
  where
    go :: Double -> Double -> [Double] -> Double
    go !sum !len []     = sum / len
    go !sum !len (y:ys) = go (sum + y) (len + 1) ys

---- compute both the mean and the variance with bang patterns
meanVariance :: [Double] -> (Double, Double)
meanVariance xs = go 0 0 0 xs
  where
    go :: Double -> Double -> Double -> [Double] -> (Double, Double)
    go !sumX !sumX2 !count [] = 
        let !n = count
            !mean = sumX / n
            !var  = sumX2 / n - mean * mean
        in (mean, var)
    go !sumX !sumX2 !count (y:ys) =
        go (sumX + y) (sumX2 + y*y) (count + 1) ys