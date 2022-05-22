import qualified Data.Map as Map
import Data.Maybe

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham", (42.1256, -70.1235))
                          ,("Minsk", (12.5555, -25.1255))
                          ,("Carcosa", (43.1252, -34.1235))
                          ,("Casablanca", (89.6712, -45.5789))]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
        where rlat = toRadians lat
              rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
        where (rlat1, rlong1) = latLongToRads coords1
              (rlat2, rlong2) = latLongToRads coords2
              dlat = rlat2 - rlat1
              dlong = rlong2 - rlong1
              a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
              c = 2 * atan2 (sqrt a) (sqrt (1-a))
              earthRadius = 3961.0

-- 28.1 haversineIO
haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO a b = do
      a <- read <$> getLine
      b <- read <$> getLine 
      c <- read <$> getLine
      d <- read <$> getLine 
      let hav = haversine (a, b) (d, c)
      return hav

-- 28.2 haversineIO
-- haversineIO :: IO LatLong -> IO LatLong -> IO Double
-- haversineIO a b = haversine <$> a <*> b

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn  "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just a) (Just b) = Just(a + b)
addMaybe _ _ = Nothing

-- problem - we need to pass two arguments in a context but functor can only work with one
-- applciative - (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- we can apply functions that are in a context, outputting value in that context
-- so this not only way of combining two values inside context but also
-- this is a way of use existing binary functions in a context
-- (++) <$> Just "cats" <*> Just " and dogs"
-- (+) <$> Just 3 <*> Just 2

val1 = Just 10
val2 = Just 5
val3 = div <$> val1 <*> val2
val4 = mod <$> val1 <*> val2

main :: IO ()
main = do
    putStrLn "Enter starting city: "
    city1 <- getLine
    putStrLn "Enter destenation city: "
    city2 <- getLine
    let coords1 = Map.lookup city1 locationDB
    let coords2 = Map.lookup city2 locationDB
    let mileage = haversine <$> coords1 <*> coords2
    maybe (putStrLn "Wrong input!") print mileage
