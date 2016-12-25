import           Control.Monad
import           Data.List
import           Data.Tuple

type Point = (Int, Int)

inf :: Double
inf = 1 / 0

readIntegers :: String -> [Int]
readIntegers = map read . words

readPoint :: String -> Point
readPoint str = (x, y)
    where [x, y] = map read (words str)

allDistances :: [Point] -> [Double]
allDistances []     = []
allDistances (x:xs) = [distance x y | y <- xs] ++ allDistances xs

distance :: Point -> Point -> Double
distance (x0, y0) (x1, y1) = sqrt . fromIntegral $ (dx*dx + dy*dy)
    where (dx, dy) = (x1 - x0, y0 - y1)

xDistance :: Point -> Point -> Double
xDistance (x, _) (x', _) = abs (fromIntegral x - fromIntegral x')

{------------------------------------------------------------------------------}

main :: IO ()
main = do
    [nPoints] <- fmap readIntegers getLine
    points <- fmap (map readPoint) (replicateM nPoints getLine)
    let xs = sort points
        ys = sortBy swapCompare points

    print (minDistance xs ys)

swapCompare :: Point -> Point -> Ordering
swapCompare x y = compare (swap x)  (swap y)

minDistance :: [Point] -> [Point] -> Double
minDistance xs ys
  | length xs <= 3 = minimum . allDistances $ xs
  | otherwise =
      let mid = length xs `div` 2
          (lXs, rXs) = splitAt mid xs
          midPoint = xs !! mid
          (lYs, rYs) = splitByX (fst midPoint) ys
          (lMin, rMin) = (minDistance lXs lYs, minDistance rXs rYs)
          minDist = min lMin rMin
       in min minDist (mergeMinDistance midPoint minDist ys)

splitByX :: Int -> [Point] -> ([Point], [Point])
splitByX cutoff ys = (reverse lYs, reverse rYs)
    where (lYs, rYs) = foldl (revAppendByX cutoff) ([], []) ys

revAppendByX :: Int -> ([Point], [Point]) -> Point -> ([Point], [Point])
revAppendByX cutoff (left, right) p@(x, _) =
    if x <= cutoff
       then (p:left, right)
       else (left, p:right)

mergeMinDistance :: Point -> Double -> [Point] -> Double
mergeMinDistance midPoint minDist ys =
    let deltaBox = filterDeltaBox midPoint minDist ys
     in minDistanceInDeltaBox minDist deltaBox

filterDeltaBox :: Point -> Double -> [Point] -> [Point]
filterDeltaBox midPoint minDist = filter ((< minDist) . xDistance midPoint)

minDistanceInDeltaBox :: Double -> [Point] -> Double
minDistanceInDeltaBox _ []     = inf
minDistanceInDeltaBox _ [_]    = inf
minDistanceInDeltaBox minDist (x:xs) =
    let minDist' = headMinDist minDist x xs
     in min minDist' (minDistanceInDeltaBox minDist xs)

headMinDist :: Double -> Point -> [Point] -> Double
headMinDist _ _ []     = inf
headMinDist minDist x (y:ys) =
    if fromIntegral (snd y - snd x) < minDist
       then min (distance x y) (headMinDist minDist x ys)
       else minDist

{- Stress Test

minDistProp :: [Point] -> Bool
minDistProp points
    | length points < 3 = True
    | otherwise =
        let xs = sort points
            ys = sortBy swapCompare xs
         in minimum (allDistances xs) == minDistance xs ys

-}
