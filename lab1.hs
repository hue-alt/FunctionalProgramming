main = greet
greet = putStrLn "Hello, World!"

r = 2
area = pi * r^2

pow::Double -> Double
pow x = x * x

vecLen::(Double, Double) -> Double
vecLen (x, y) = sqrt(x^2 + y^2)


swap::(a, b) -> (b, a)
swap (x, y) = (y, x)


vec3Dlen::(Double, Double, Double) -> Double
vec3Dlen(x, y, z) = sqrt(x^2 + y^2 + z^2)

sgn::Int -> Int
sgn x = if x < 0
        then -1
        else if x > 0
        then 1
        else 0

min_val (c, d) = if c < d
            then c
            else d

absInt w
    | w > 0 = w
    | w < 0 = -w
    | otherwise = 0

sgn' x
    | x < 0 = -1
    | x > 0  = 1
    |otherwise = 0
    

and' ::(Bool, Bool) -> Bool
and' (True, True) = True
and'(_, _) = False

or' ::(Bool, Bool) -> Bool
or' (False, False) = False
or' (_, _) = True

xor' ::(Bool, Bool) -> Bool
xor' (True, True) = False
xor' (False, False) = False
xor' (_, _) = True

roots_polynomial :: (Double, Double, Double) -> (Double, Double)
roots_polynomial (a, b, c) = ((-b - d) / e, (-b + d) / e)
  where
    d = sqrt (b * b - 4 * a * c)
    e = 2 * a


roots_polynomial' :: (Double, Double, Double) -> (Double, Double)
roots_polynomial'(a, b, c) = 
    let d = sqrt(b^2 - 4 * a * c)
        e = 2 * a
    in ((-b - d) / e, (-b + d) / e)



collatz :: Int -> Int
collatz n = if n == 1
            then 1
            else if n `mod` 2 == 0
            then collatz (n `div` 2)
            else collatz (3 * n + 1)

collatz' :: Int -> Int
collatz' n
    | n == 1 = 1
    | n `mod` 2 == 0 = collatz' (n `div` 2)
    | otherwise = collatz' (3 * n + 1)

