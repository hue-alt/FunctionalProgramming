import Data.Char (toUpper, toLower)
import Data.List (foldl', sort)

roundValue :: Double -> Double
roundValue x = fromIntegral (round (x * 100) :: Int) / 100.0

capitalize :: String -> String
capitalize []       = []
capitalize (c : cs) = toUpper c : map toLower cs

formatTitle :: String -> String
formatTitle = unwords . map capitalize . filter ((> 1) . length) . words

makeDiscount :: Double -> (Double -> Double)
makeDiscount rate = \price -> roundValue (price * (1.0 - rate))

chooseTicketOp :: String -> (Double -> Int -> Double)
chooseTicketOp kind = case kind of
  "normal"  -> \price count -> price * fromIntegral count
  "student" -> \price count -> price * 0.5 * fromIntegral count
  "vip"     -> \price count -> (price + 10.0) * fromIntegral count
  _         -> \_ _ -> 0.0

hallRevenues :: [Double] -> [Int] -> [Double]
hallRevenues = zipWith (\price viewers -> roundValue (price * fromIntegral viewers))

totalRevenue :: [Double] -> Double
totalRevenue = roundValue . foldl' (+) 0.0

countPopular :: Int -> [Int] -> Int
countPopular minViewers = foldr (\v acc -> if v >= minViewers then acc + 1 else acc) 0

applyPromos :: Double -> [Double -> Double] -> [Double]
applyPromos basePrice = map ($ basePrice)

topRatingsDesc :: [Double] -> [Double]
topRatingsDesc = reverse . sort . filter (>= 7.0)

main :: IO ()
main = do
  putStrLn "=== CINEMA SYSTEM ==="
  let rawTitle = "  the a matrix reloaded x  "
      ticketPrices = [25.0, 22.0, 30.0, 18.0]
      viewers = [120, 85, 200, 60]
      ratings = [8.7, 6.4, 9.1, 7.5, 5.9]
      revenues = hallRevenues ticketPrices viewers
      promos = [makeDiscount 0.1, makeDiscount 0.25, \p -> roundValue (p - 5.0)]

  putStrLn ("Featured movie: " ++ formatTitle rawTitle)
  putStrLn ("Hall revenues: " ++ show revenues)
  putStrLn ("Total revenue: " ++ show (totalRevenue revenues) ++ " PLN")
  putStrLn ("Popular screenings (>= 100): " ++ show (countPopular 100 viewers))
  putStrLn ("Top ratings: " ++ show (topRatingsDesc ratings))
  putStrLn ("Student tickets (3x 28 PLN): " ++ show (chooseTicketOp "student" 28.0 3) ++ " PLN")
  putStrLn ("Promo prices from 30 PLN: " ++ show (applyPromos 30.0 promos))
