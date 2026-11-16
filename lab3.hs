import Data.Char (toUpper, toLower)
import Data.List (sort, foldl')

add1, add2, add3, add4 :: Num a => a -> a -> a
add1 = \x -> \y -> x + y
add2 = \x y -> x + y
add3 x = \y -> x + y
add4 x y = x + y

addT :: Num a => (a, a) -> a
addT = \(x, y) -> x + y

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith _ []       = 0
sumWith f (x : xs) = f x + sumWith f xs

sumSq, sumCube, sumAbs :: Num a => [a] -> a
sumSq = sumWith (\x -> x * x)
sumCube = sumWith (\x -> x * x * x)
sumAbs = sumWith abs

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyN :: Int -> (a -> a) -> a -> a
applyN n f x
  | n <= 0    = x
  | otherwise = applyN (n - 1) f (f x)

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

doubleAll :: Num a => [a] -> [a]
doubleAll = map (* 2)

squareAll :: Num a => [a] -> [a]
squareAll = map (\x -> x * x)

selectEven :: Integral a => [a] -> [a]
selectEven = filter even

lengths :: [String] -> [Int]
lengths = map length

isSorted :: Ord a => [a] -> Bool
isSorted xs = and (zipWith (<=) xs (drop 1 xs))

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

everySecond :: [a] -> [a]
everySecond xs = [x | (i, x) <- zip [0 :: Int ..] xs, even i]

diffs :: Num a => [a] -> [a]
diffs xs = zipWith (-) (drop 1 xs) xs

positions :: Eq a => a -> [a] -> [Int]
positions y xs = [i | (i, x) <- zip [0 ..] xs, x == y]

sortDesc :: Ord a => [a] -> [a]
sortDesc = reverse . sort

countEvenSq :: [Int] -> Int
countEvenSq = length . filter even . map (\x -> x * x)

sumEvenSq :: [Int] -> Int
sumEvenSq = sum . map (\x -> x * x) . filter even

countLongWords :: [String] -> Int
countLongWords = length . filter ((> 3) . length)

applyTwice' :: (a -> a) -> a -> a
applyTwice' f = f . f

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z []       = z
myFoldr f z (x : xs) = f x (myFoldr f z xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc []       = acc
myFoldl f acc (x : xs) = myFoldl f (f acc x) xs

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

prod' :: Num a => [a] -> a
prod' = foldr (*) 1

length' :: [a] -> Int
length' = foldr (\_ acc -> acc + 1) 0

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

and' :: [Bool] -> Bool
and' = foldr (&&) True

maximum' :: Ord a => [a] -> a
maximum' []       = error "empty list"
maximum' (x : xs) = foldl max x xs

sumSqFold :: Num a => [a] -> a
sumSqFold = foldr (\x acc -> x * x + acc) 0

countIf :: (a -> Bool) -> [a] -> Int
countIf p = foldr (\x acc -> if p x then acc + 1 else acc) 0

addN :: Num a => a -> (a -> a)
addN n = \x -> x + n

addN' :: Num a => a -> a -> a
addN' n x = x + n

derivative :: (Double -> Double) -> Double -> (Double -> Double)
derivative f h = \x -> (f (x + h) - f (x - h)) / (2 * h)

chooseOp :: String -> (Int -> Int -> Int)
chooseOp name = case name of
  "add" -> (+)
  "mul" -> (*)
  "max" -> max
  _     -> \_ _ -> 0

funcList :: [Double -> Double]
funcList = [\x -> x * x, sqrt, (+ 1)]

applyAll :: a -> [a -> b] -> [b]
applyAll x = map ($ x)

capitalize :: String -> String
capitalize []       = []
capitalize (c : cs) = toUpper c : map toLower cs

formatNames :: String -> String
formatNames = unwords . map capitalize . filter ((> 1) . length) . words

startsWithVowel :: String -> Bool
startsWithVowel (c : _) = c `elem` "aąeęiouóyAĄEĘIOUÓY"
startsWithVowel []      = False

countWordsGt3 :: String -> Int
countWordsGt3 = length . filter ((> 3) . length) . words

sumVowelWordLengths :: String -> Int
sumVowelWordLengths = sum . map length . filter startsWithVowel . words

div7SquaresDesc :: [Int]
div7SquaresDesc = sortDesc . map (\x -> x * x) . filter (\x -> x `mod` 7 == 0) $ [1 .. 100]

avgPositive :: [Double] -> Double
avgPositive xs = sum pos / fromIntegral (length pos)
  where
    pos = filter (> 0) xs
