import           Control.Monad
import           Data.List

main :: IO ()
main = do
    [nPoints] <- fmap readIntegers getLine
    points <- fmap (sort . map readPoint) (replicateM nPoints getLine)
    print (minDistance points)

type Point = (Int, Int)

inf :: Double
inf = 1 / 0

readIntegers :: String -> [Int]
readIntegers = map read . words

readPoint :: String -> Point
readPoint str = (x, y)
    where [x, y] = map read (words str)

distance :: Point -> Point -> Double
distance (x0, y0) (x1, y1) = sqrt . fromIntegral $ (dx*dx + dy*dy)
    where (dx, dy) = (x1 - x0, y0 - y1)

minDistance :: [Point] -> Double
minDistance []  = inf
minDistance [_] = inf
minDistance [x, y] = distance x y
minDistance xs  =
    let mid = div (length xs) 2
        (lPoints, rPoints) = splitAt mid xs
        (lMin, rMin) = (minDistance lPoints, minDistance rPoints)
        minDist = min lMin rMin
     in mergeMinDistance (lPoints, rPoints) minDist

mergeMinDistance :: ([Point], [Point]) -> Double -> Double
mergeMinDistance (lPoints, rPoints) minDist =
    let minDistFromMerge = minDistanceFromTwoSets minDist lPoints rPoints
     in min minDistFromMerge minDist

minDistanceFromTwoSets :: Double -> [Point] -> [Point] -> Double
minDistanceFromTwoSets _ [] _ = inf
minDistanceFromTwoSets _ _ [] = inf
minDistanceFromTwoSets minDist lPoints (p:ps) =
    let edge = fromIntegral . fst $ p
        lPointsToConsider = eliminatePoints minDist edge lPoints
        distances = map (distance p) lPointsToConsider
        minDist' = if null distances then inf else minimum distances
     in min minDist' (minDistanceFromTwoSets minDist lPointsToConsider ps)

eliminatePoints :: Double -> Double -> [Point] -> [Point]
eliminatePoints minDist edge points =
    let xDistFromClosest (x, _) = abs (fromIntegral x - edge)
     in filter ((< minDist) . xDistFromClosest) points

{-
xs = [(-4,0),(-4,2),(-3,-4),(-2,-2),(-2,4),(-1,-1),(-1,3),(1,1),(2,3),(3,-1),(4,4)]
xs = [(1,100),(4,8),(7,7),(7,7)]
-}
