-- Exercise 1
fac :: Int -> Int
fac 0 = 1
fac n   | n < 0 = error "Negative number"
        | otherwise = n * fac (n-1)

-- Exercise 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n-1)

-- Exercise 3
-- Renamed so I don't run into namespace ambiguity later on
exp' :: Int -> Int -> Int
exp' x 0 = 1
exp' x n | n > 0 =  x * ( exp' x (n-1) )

-- Exercise 4
-- Note: instructions weren't totally clear. "Process is repeated" 
-- means the function is called on the smaller number and the new calculated number.
euclid :: Int -> Int -> Int
euclid x y  | x == y = x
            | x > y = euclid (x - y) y
            | x < y = euclid x (y - x)

-- Exercise 5
{-
length [1,2,3]
    1 + length [2,3]
    1 + 1 + length [3]
    1 + 1 + 1 + length []
    1 + 1 + 1 + 0
    3

drop 3 [1..5]
    drop 2 [2..5]
    drop 1 [3,4,5]
    drop 0 [4,5]
    [4,5]

init [1,2,3]
    1 : init [2,3]
    1 : 2 : init [3]
    1 : 2 : []
    [1,2]
-}

-- Exercise 6
-- Again, renamed so I don't run into namespace ambiguity later on
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (l:ls) = l ++ concat' ls

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' len val = val : replicate' (len-1) val

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! i | i > 0 = xs !!! (i-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' val (x:xs)    | val == x = True
                    | otherwise = elem' val xs

-- Exercise 7
-- Merge presorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x >= y    = y : merge (x:xs) ys
                    | otherwise = x : merge xs (y:ys)
merge [] l = l
merge l [] = l

-- Exercise 8
halve :: [a] -> ([a],[a])
halve xs = splitAt ((length xs) `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort firstHalf) (msort secondHalf)
    where
        (firstHalf, secondHalf) = halve xs

-- Exercise 9
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) | n > 0 = x : take' (n-1) xs

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last' xs