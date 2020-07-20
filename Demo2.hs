data Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = (Coord x' y')
  where
    halfOfSize = size / 2
    x' = (fromIntegral x * size) + halfOfSize
    y' = (fromIntegral y * size) + halfOfSize


getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = (Coord x' y')
  where
    x' = (floor (x / size))
    y' = (floor (y / size))
