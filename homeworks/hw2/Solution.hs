import Data.Foldable (toList)
main :: IO ()
main = do
    putStrLn "Homework 2!"
    let nySequence :: Sequence Int = Append (Single 1) (Append (Single 2) (Single 3))
    print ("Ex 2: ", seqToList nySequence)
    print ("Ex 2: ", seqLength nySequence)
    let mySequence2 :: Sequence Int = Append (Single 1) (Append Empty (Append (Single 4) (Single 3)))
    print("Ex 4: Sequence: ", seqToList mySequence2)
    print("Ex 4: Is there 4 in the sequence? ", tailElem 4 mySequence2)
    print("Ex 4: Is there 2 in the sequence? ", tailElem 2 mySequence2)
    print("Ex 4: Is there 7 in the sequence?", tailElem 7 mySequence2)
    let mySequence3 :: Sequence Int = Append (Single 1) (Append (Single 2) (Append (Single 3) (Single 4)))
    print("Ex 5: Flattened sequence: ", tailToList mySequence3)
    let rpnTokens :: [Token] = [TNum 3, TNum 4, TAdd, TNum 12, TMul]
    print("Ex 6: RPN evaluation of [3, 4, +, 12, *]: ", tailRPN rpnTokens)
    let rpnTokens2 :: [Token] = [TNum 555, TNum 0, TDiv]
    print("Ex 6 dividing by zero: RPN evaluation of [555, 0, /]: ", tailRPN rpnTokens2)
    let rpnTokens3 :: [Token] = [TNum 10, TNum 5, TSub, TNum 2, TNum 3, TMul]
    print("Ex 6 too many numbers: RPN evaluation of [10, 5, -, 2, 3, *]: ", tailRPN rpnTokens3)
    let rpnTokens4 :: [Token] = [TSub, TNum 2, TNum 3, TMul]
    print("Ex 6 too few numbers: RPN evaluation of [TSub, TNum 2, TNum 3, TMul]: ", tailRPN rpnTokens4)
    print("Ex 7 a: myReverse of [1, 2, 3, 4, 5]: ", myReverse [1, 2, 3, 4, 5])
    print("Ex 7 b: myTakeWhile (< 4) of [1, 2, 3, 4, 5]: ", myTakeWhile (< 4) [1, 2, 3, 4, 5])
    print("Ex 7 c: decimal of [7, 7, 3, 4]: ", decimal [7, 7, 3, 4])
    print("Ex 8 a: encode of aaabccca: ", encode "aaabccca")
    print("Ex 8 b: decode of [('a',3),('b',1),('c',3),('a',1)]: ", decode [('a',3),('b',1),('c',3),('a',1)])


data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)

-- 1. Functor for Sequence
instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- 2. Foldable for Sequence
instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

-- 3. Semigroup and Monoid for Sequence
instance Semigroup (Sequence a) where
    (<>) :: Sequence a -> Sequence a -> Sequence a
    Empty <> y = y
    x <> Empty = x 
    x <> y = Append x y

instance Monoid (Sequence a) where
    mempty :: Sequence a
    mempty = Empty

-- 4. Tail Recursion and Sequence Search
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem trg seq = go [seq]
    where
        go [] = False
        go (Empty: rest) = go rest
        go (Single x : rest)
            | x == trg = True
            | otherwise = go rest
        go (Append l r : rest) = go (l : r : rest)

-- 5. Tail Recursion and Sequence Flatten
tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
    where 
        go :: [Sequence a] -> [a] -> [a]
        go [] acc = acc
        go (Empty : rest) acc = go rest acc 
        go (Single x : rest) acc = go rest (x : acc)
        go (Append l r : rest) acc = go (r : l : rest) acc

-- 6. Tail Recursion and Reverse Polish Notation
data Token = TNum Int | TAdd | TSub | TMul | TDiv 

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
    where
        go :: [Token] -> [Int] -> Maybe Int
        go [] [result] = Just result
        go [] _ = Nothing
        go (TNum n: ts) stack = go ts (n: stack)

        go (TAdd : ts) (y:x:stackRest) = go ts ((x + y) : stackRest)
        go (TSub : ts) (y:x:stackRest) = go ts ((x - y) : stackRest)
        go (TMul : ts) (y:x:stackRest) = go ts ((x * y) : stackRest)

        go (TDiv : ts) (y:x:stackRest)
            | y == 0 = Nothing
            | otherwise = go ts ((x `div` y) : stackRest)

        go _ _ = Nothing

-- 7. Expressing functions via foldr and foldl

-- a)
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

-- b)
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

-- c)
decimal :: [Int] -> Int
decimal = foldl (\acc x -> acc * 10 + x) 0

-- 8. Run-length encoding via folds

-- a)
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
    where
        step x [] = [(x, 1)]
        step x ((y, n) :  rest)
            | x == y    = (y, n + 1) : rest
            | otherwise = (x, 1) : (y, n) : rest

-- b)
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []