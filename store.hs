roundValue :: Double -> Double
roundValue x = fromIntegral (round (x * 100) :: Int) / 100.0

discountedPrices :: Double -> [Double] -> [Double]
discountedPrices rate prices = [roundValue (p * (1.0 - rate)) | p <- prices, p > 0]

affordableItems :: Double -> [(String, Double)] -> [String]
affordableItems budget items = [name | (name, price) <- items, price <= budget]

totalPrice :: [Double] -> Double
totalPrice = loop 0.0
  where
    loop !acc []       = roundValue acc
    loop !acc (p : ps) = loop (acc + p) ps

countExpensive :: Double -> [Double] -> Int
countExpensive _ [] = 0
countExpensive threshold (p : ps)
  | p > threshold = 1 + countExpensive threshold ps
  | otherwise     = countExpensive threshold ps

isSortedPrices :: [Double] -> Bool
isSortedPrices (x : y : rest) = x <= y && isSortedPrices (y : rest)
isSortedPrices _              = True

qSortPrices :: [Double] -> [Double]
qSortPrices []       = []
qSortPrices (p : ps) = qSortPrices smaller ++ [p] ++ qSortPrices larger
  where
    smaller = filter (<= p) ps
    larger  = filter (> p) ps

rleCategories :: [String] -> [(String, Int)]
rleCategories [] = []
rleCategories (c : cs) = (c, 1 + length same) : rleCategories rest
  where
    (same, rest) = span (== c) cs

main :: IO ()
main = do
  putStrLn "=== STORE SYSTEM ==="
  let catalog = [("Bread", 4.5), ("Milk", 3.8), ("Coffee", 29.9), ("Cheese", 14.5), ("Tea", 12.0)]
      prices = [p | (_, p) <- catalog]
      sorted = qSortPrices prices
      sale = discountedPrices 0.15 sorted
      categories = ["Food", "Food", "Drink", "Food", "Drink"]

  putStrLn ("Sorted prices: " ++ show sorted)
  putStrLn ("Already sorted: " ++ show (isSortedPrices prices))
  putStrLn ("Discounted prices: " ++ show sale)
  putStrLn ("Affordable items: " ++ show (affordableItems 15.0 catalog))
  putStrLn ("Items above 10 PLN: " ++ show (countExpensive 10.0 prices))
  putStrLn ("Category runs: " ++ show (rleCategories categories))
  putStrLn ("Total sale value: " ++ show (totalPrice sale) ++ " PLN")
