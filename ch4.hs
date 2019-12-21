-- Ex. 1
halve xs = splitAt (div (length xs) 2) xs

-- Ex. 2 
thirdA xs = head (tail (tail xs))
thirdB xs = xs !! 2
thirdC (_:_:x:_) = x

-- Ex. 3
safetailA xs = if null xs then [] else tail xs
safetailB xs | null xs = []
             | otherwise = tail xs
safetailC (_:xs) = xs
safetailC _ = []

-- Ex. 4
-- v1
True || True = True
True || False = True
False || True = True
False || False = False
--v2
False || False = False
_ || _ = True
--v3
False || b = b
True || _ = True
--v4
b || c 
    | b==c = b
    | otherwise = True    

-- Ex. 5
(&&) x y = if x then if y then True else False else False

-- Ex. 6
(&&) x b = if x then b else False

-- Ex. 7
mult = \x -> (\y -> (\z -> x * y * z)) 

-- Ex. 8
luhnDouble x = if (y > 9) then y-9 else y
    where y = 2*x
luhn a b c d = ((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10 == 0