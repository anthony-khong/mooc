import           Control.Monad
import           Data.List
import           Data.Tuple

type Point = (Int, Int)
type PointIx = (Point, Int)

inf :: Double
inf = 1 / 0

readIntegers :: String -> [Int]
readIntegers = map read . words

readPoint :: String -> Point
readPoint str = (x, y)
    where [x, y] = map read (words str)

{------------------------------------------------------------------------------}
{-

points = [(-4,0),(-4,2),(-3,-4),(-2,-2),(-2,4),(-1,-1),(-1,3),(1,1),(2,3),(3,-1),(4,4)]
points = [(7,7),(1,100),(4,8),(7,7)]

points = [(1,0),(0,4),(-1,0),(1,3)]
xs = sort points
ys = sortBy compareYCoordinate (zip xs [0..])
start = 0
end = length xs - 1
minDist = 3.605551275463989
(lIxs, rIxs) = selectEligibleIxs (start, end) xs minDist
(lYs, rYs) = (selectByIx lIxs ys, selectByIx rIxs ys)
-}

main :: IO ()
main = do
    [nPoints] <- fmap readIntegers getLine
    points <- fmap (map readPoint) (replicateM nPoints getLine)
    let xs = sort points
        ys = sortBy compareYCoordinate (zip xs [0..])

    print . minimum . allDistances $ xs
    print (minDistance xs ys)

compareYCoordinate :: PointIx -> PointIx -> Ordering
compareYCoordinate (p, _) (p', _) = compare (swap p) (swap p')

distance :: Point -> Point -> Double
distance (x0, y0) (x1, y1) = sqrt . fromIntegral $ (dx*dx + dy*dy)
    where (dx, dy) = (x1 - x0, y0 - y1)

xDistance :: Point -> Point -> Double
xDistance (x, _) (x', _) = abs (fromIntegral x - fromIntegral x')

{-yDistance :: Point -> Point -> Double-}
{-yDistance (_, y) (_, y') = abs (fromIntegral y - fromIntegral y')-}

allDistances :: [Point] -> [Double]
allDistances []     = []
allDistances (x:xs) = [distance x y | y <- xs] ++ allDistances xs

minDistance :: [Point] -> [PointIx] -> Double
minDistance xs = recMinDistance (0, length xs - 1) xs

recMinDistance :: (Int, Int) -> [Point] -> [PointIx] -> Double
recMinDistance (start, end) xs ys
    | end - start == 1 = distance (xs !! start) (xs !! end)
    | end <= start = inf
    | otherwise =
        let mid = div (start + end) 2
            lMin = recMinDistance (start, mid) xs ys
            rMin = recMinDistance (mid + 1, end) xs ys
            minDist = min lMin rMin
         in mergeMinDistance (start, end) xs ys minDist

mergeMinDistance :: (Int, Int) -> [Point] -> [PointIx] -> Double -> Double
mergeMinDistance (start, end) xs ys minDist =
    let (lIxs, rIxs) = selectEligibleIxs (start, end) xs minDist
        (lYs, rYs) = (selectByIx lIxs ys, selectByIx rIxs ys)
     in min minDist (minDistInBox lYs rYs minDist)

selectEligibleIxs :: (Int, Int) -> [Point] -> Double -> ([Int], [Int])
selectEligibleIxs (start, end) xs minDist =
    let mid = div (start + end) 2
        lIxs = filter (lessThanDelta (mid + 1)) [start..mid]
        rIxs = filter (lessThanDelta mid) [(mid+1)..end]
        lessThanDelta edgeIx n = xDistance (xs !! edgeIx) (xs !! n) < minDist
     in (lIxs, rIxs)

selectByIx :: [Int] -> [PointIx] -> [Point]
selectByIx ixs = map fst . filter inIxs
    where inIxs (_, ix) = ix `elem` ixs

minDistInBox :: [Point] -> [Point] -> Double -> Double
minDistInBox [] _ _ = inf
minDistInBox (x:xs) ys minDist =
    let headMinDist = minimalMinDist x ys minDist
     in min headMinDist (minDistInBox xs ys minDist)

minimalMinDist :: Point -> [Point] -> Double -> Double
minimalMinDist _ [] _ = inf
minimalMinDist x (y:ys) minDist =
    if fromIntegral (snd y - snd x) <= minDist
       then min (distance x y) (minimalMinDist x ys minDist)
       else minDist

{------------------------------------------------------------------------------}

{-minDistProp :: [Point] -> Bool-}
{-minDistProp points-}
    {-| length points < 3 = True-}
    {-| otherwise =-}
        {-let xs = sort points-}
            {-ys = sortBy compareYCoordinate (zip xs [0..])-}
         {-in minimum (allDistances xs) == minDistance xs ys-}

{-

import           Test.QuickCheck
quickCheck minDistProp

-}

{-minDistInBox :: [Point] -> [Point] -> Double -> Double-}
{-minDistInBox [] _ _ = inf-}
{-minDistInBox _ [] _ = inf-}
{-minDistInBox xs ys minDist =-}
    {-let minDist' = minimum [distance x y | x <- xs, y <- ys]-}
     {-in min minDist minDist'-}
