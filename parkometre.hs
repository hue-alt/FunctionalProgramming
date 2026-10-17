minutesToHours :: Int -> Double
minutesToHours minutes = fromIntegral minutes / 60.0

vehicleCheck :: (Bool, Bool) -> String
vehicleCheck (True, _)      = "Single-track vehicle"
vehicleCheck (False, True)  = "Electric vehicle"
vehicleCheck (False, False) = "Other vehicle"

howManyToPay :: Double -> Double
howManyToPay time
  | time <= 0.5 = 0.0
  | time <= 1.0 = 1.5
  | time <= 2.0 = 3.0
  | otherwise   = 5.0


receipt :: Double -> Double
receipt price_all = netto + vat
  where
    netto = price_all
    vat   = netto * 0.23


parkingMachine :: (Double, Double) -> String
parkingMachine (fee, price) = 
  if fee - price > 0
  then "Your change is: " ++ show (fee - price) ++ " PLN"
  else if fee - price == 0
  then "Thank you for your payment"
  else "You need to pay: " ++ show (price - fee) ++ " PLN"


screenDisplay :: Int -> String
screenDisplay code = case code of
  0 -> "Ready"
  1 -> "Processing payment"
  _ -> "Error"


round2dp :: Double -> Double
round2dp x = fromIntegral (round (x * 100)) / 100.0


main :: IO ()
main = do
  putStrLn "=== PARKING METER SYSTEM ==="
  putStrLn ("Status: " ++ screenDisplay 0)

  let myVehicle = (False, False)
  putStrLn ("Vehicle type: " ++ vehicleCheck myVehicle)

  let minutes = 90
      hours = minutesToHours minutes
      baseRate = howManyToPay hours
      totalPrice = receipt baseRate
      insertedMoney = 10.0

  putStrLn ("Parking duration: " ++ show hours ++ " h")
  putStrLn ("Total price (incl. VAT): " ++ show totalPrice ++ " PLN")
  putStrLn ("Your change: " ++ show (round2dp (insertedMoney - totalPrice)) ++ " PLN")