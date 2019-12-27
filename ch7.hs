import Data.Char

type Bit = Int

-- Exercise 1
-- map f . filter p xs

-- Exercise 2
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs else x:xs

-- Exercise 3
map' f = foldr (\x xs -> f x : xs) []
filter' p = foldr (\x xs -> if p x then x:xs else xs ) []

-- Exercise 4
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> x + 10*acc) 0

-- Exercise 5
curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = \(x,y) -> f x y

-- Exercise 6
unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (==[]) (take 8) (drop 8)

map'' f = unfold (==[]) (f . head) tail

iterate' f = unfold (const False) id f

-- Exercise 7
-- Unchanged

bin2int :: [Bit] -> Int
bin2int = foldr (\x acc -> x + 2*acc) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Changed / new

makeParity :: [Bit] -> Bit
makeParity bits = if odd (sum bits) then 1 else 0

addParity :: [Bit] -> [Bit]
addParity bits = bits ++ [makeParity bits]

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

filterByte :: [Bit] -> [Bit]
filterByte bits | makeParity byte == last bits = byte
                | otherwise = error "parity error"
                where byte = take 8 bits

decode :: [Bit] -> String
decode = map (chr . bin2int . filterByte) . chop9

-- Exercise 8
faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode

-- Exercise 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

-- Exercise 10
-- Works out to be the same as ch4.hs luhnDouble
luhnDouble x = 2*x `mod` 9

-- since we no longer have only 4 numbers, we can't rely on position like ch4.hs
-- We need to reverse the card number to work right-to-left
luhn :: [Int] -> Bool
luhn cardNum = sum (altMap id luhnDouble (reverse cardNum)) `mod` 10 == 0
