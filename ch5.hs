import Data.Char

-- Exercise 1
ex1 = sum [x^2 | x <- [1..100]]

-- Exercise 2
grid xmax ymax = [ (x,y) | x <- [0..xmax], y <- [0..ymax]]

-- Exercise 3
square n = [ (x,y) | (x,y) <- grid n n, x /= y]

-- Exercise 4
replicate len val = [ val | _ <- [1..len]]

-- Exercise 5
pyths n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Exercise 6
factors n = [x | x <- [1..n], n `mod` x == 0]
perfects limit = [x | x <- [1..limit], sum (init (factors x)) == x]

-- Exercise 7
ex7 = concat [ [ (x,y) | y <- [3,4] ] | x <- [1,2] ] 

-- Exercise 8
find k t = [v | (k',v) <- t, k == k']
positions x xs = find x (zip xs [0..])

-- Exercise 9
scalarproduct xs ys = sum [ xi*yi | (xi, yi) <- zip xs ys]

-- Exercise 10
-- if ord c + n is larger than ord 'z' (122), then wrap back starting from ord 'a' (97)
-- ex. ord "z" + 2 = 124. 124-97 mod 26 = 1. chr (1 + 97)  = 'b'
shift n c | isLower c = chr( (ord c + n - ord 'a') `mod` 26 + ord 'a' )
          | isUpper c = chr( (ord c + n - ord 'A') `mod` 26 + ord 'A' )
          | otherwise = c 
encode n xs = [shift n x | x <- xs]