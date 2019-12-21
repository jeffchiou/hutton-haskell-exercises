-- 1 and 2 are given.
-- Exercise 3
second :: [a] -> a
second xs = head (tail xs)
swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)
pair :: a -> b -> (a,b)
pair x y = (x,y)
double :: Num a => a -> a
double x = x * 2
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Exercise 4
-- use :t

-- Exercise 5
{- 
Why is it not feasible in general for function types to be instances of the Eq class?
Functions are hard to compare - mathematically impossible.

When is it feasible?
If each of possible inputs (a finite domain) return the same corresponding output for both functions,
or the functions are alpha-equivalent.
-}