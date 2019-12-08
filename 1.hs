import Data.Either

main :: IO ()
main = do
  modules <- openFile
  let modulesMass = sum $ calculateFuelForWeight . read <$> lines modules
      fuelMass = sum $ addFuelWeight . read <$> lines modules
  print (modulesMass, fuelMass)

calculateFuelForWeight :: Double -> Int
calculateFuelForWeight a = (floor (a / 3)) - 2

addFuelWeight :: Int -> Int
addFuelWeight initial
  | additionalFuel > 0 = additionalFuel + addFuelWeight additionalFuel
  | otherwise = 0
  where additionalFuel = calculateFuelForWeight (fromIntegral initial)


openFile :: IO String
openFile = do
  readFile "1.data"
