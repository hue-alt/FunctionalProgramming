import Data.Char (isLower)

pairs5 :: [(Int, Int)]
pairs5 = [(i, j) | i <- [1 .. 4], j <- [1 .. 4], i < j, i + j == 5]

vowels :: Int
vowels = length [c | c <- "Programowanie funkcyjne", c `elem` "aąeęiouóyAĄEĘIOUÓY"]

lowers :: String -> Int
lowers xs = length [c | c <- xs, isLower c]

isqrt :: Int -> Int
isqrt n = floor (sqrt (fromIntegral n :: Double))

isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | otherwise = null [i | i <- [2 .. isqrt n], n `mod` i == 0]

answer :: Int -> Int -> Int
answer x y = if x > 0 then 42 else x + y

and2 :: Bool -> Bool -> Bool
and2 False _ = False
and2 True y  = y

if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

primes :: [Int]
primes = sieve [2 ..]
  where
    sieve []       = []
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

fact :: Integer -> Integer
fact n
  | n <= 1    = 1
  | otherwise = n * fact (n - 1)

fib :: Int -> Integer
fib n
  | n <= 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 2) + fib (n - 1)

fibLin :: Int -> Integer
fibLin n = go n 0 1
  where
    go 0 a _ = a
    go k a b = go (k - 1) b (a + b)

sum' :: Num a => [a] -> a
sum' []       = 0
sum' (x : xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' []       = 1
prod' (x : xs) = x * prod' xs

length' :: [a] -> Int
length' []       = 0
length' (_ : xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' []       = False
or' (x : xs) = x || or' xs

and' :: [Bool] -> Bool
and' []       = True
and' (x : xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ []       = False
elem' y (x : xs) = y == x || elem' y xs

maximum' :: Ord a => [a] -> a
maximum' []       = error "empty list"
maximum' [x]      = x
maximum' (x : xs) = max x (maximum' xs)

doubleAll :: Num a => [a] -> [a]
doubleAll []       = []
doubleAll (x : xs) = (2 * x) : doubleAll xs

squareAll :: Num a => [a] -> [a]
squareAll []       = []
squareAll (x : xs) = (x * x) : squareAll xs

selectEven :: Integral a => [a] -> [a]
selectEven [] = []
selectEven (x : xs)
  | even x    = x : selectEven xs
  | otherwise = selectEven xs

fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

divides :: [Int] -> Bool
divides (x : y : _) | x /= 0 = y `mod` x == 0
divides _                    = False

isSorted :: Ord a => [a] -> Bool
isSorted (x : y : rest) = x <= y && isSorted (y : rest)
isSorted _              = True

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
  where
    loop acc []       = acc
    loop acc (y : ys) = loop (acc + y) ys

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
  where
    loop acc []       = acc
    loop acc (y : ys) = loop (acc + y) ys

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
  where
    loop !acc []       = acc
    loop !acc (y : ys) = loop (acc + y) ys

prod'2 :: Num a => [a] -> a
prod'2 = loop 1
  where
    loop !acc []       = acc
    loop !acc (y : ys) = loop (acc * y) ys

length'2 :: [a] -> Int
length'2 = loop 0
  where
    loop !acc []       = acc
    loop !acc (_ : ys) = loop (acc + 1) ys

reverse1 :: [a] -> [a]
reverse1 []       = []
reverse1 (x : xs) = reverse1 xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 = loop []
  where
    loop acc []       = acc
    loop acc (x : xs) = loop (x : acc) xs

areEven :: Integral a => [a] -> [Bool]
areEven = loop []
  where
    loop acc []       = reverse acc
    loop acc (x : xs) = loop (even x : acc) xs

qSort :: Ord a => [a] -> [a]
qSort []       = []
qSort (x : xs) = qSort smaller ++ [x] ++ qSort larger
  where
    smaller = filter (<= x) xs
    larger  = filter (> x) xs

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y : ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert' x ys

iSort :: Ord a => [a] -> [a]
iSort []       = []
iSort (x : xs) = insert' x (iSort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y    = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

mSort :: Ord a => [a] -> [a]
mSort []  = []
mSort [x] = [x]
mSort xs  = merge (mSort left) (mSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

fizzBuzz :: Int -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0  = "Fizz"
  | n `mod` 5 == 0  = "Buzz"
  | otherwise       = show n

gcd' :: Int -> Int -> Int
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

sumDigits :: Int -> Int
sumDigits n
  | n < 0     = sumDigits (abs n)
  | n < 10    = n
  | otherwise = (n `mod` 10) + sumDigits (n `div` 10)

nub' :: Eq a => [a] -> [a]
nub' []       = []
nub' (x : xs) = x : nub' [y | y <- xs, y /= x]

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count y (x : xs)
  | y == x    = 1 + count y xs
  | otherwise = count y xs

isEven :: Int -> Bool
isEven n
  | n < 0     = False
  | n == 0    = True
  | otherwise = isOdd (n - 1)

isOdd :: Int -> Bool
isOdd n
  | n <= 0    = False
  | otherwise = isEven (n - 1)

ackermann :: Int -> Int -> Int
ackermann m n
  | m == 0    = n + 1
  | n == 0    = ackermann (m - 1) 1
  | otherwise = ackermann (m - 1) (ackermann m (n - 1))

rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x : xs) = (x, 1 + length same) : rle rest
  where
    (same, rest) = span (== x) xs

unrle :: [(a, Int)] -> [a]
unrle xs = concat [replicate n x | (x, n) <- xs]
