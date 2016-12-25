import           Control.Monad
import qualified Data.Array    as A
import           Data.List

type Arr a = A.Array Int a

main :: IO ()
main = do
    [nSegments, _] <- fmap parseIntegers getLine
    segments <- fmap parseSegments (replicateM nSegments getLine)
    points <- fmap parseIntegers getLine
    let pairs = toSPPairs segments points
        pointsAsPairs = map (\x -> (x, Point)) points
        cumSegmentCount = scanl countSegment 0 pairs

    let (aPairs, aCumSegmentCount) = (toArray pairs, toArray cumSegmentCount)
        pointsIx = map (binarySearch aPairs) pointsAsPairs
        output = fmap (aCumSegmentCount A.!) pointsIx

    putStrLn . unwords . map show $ output

type Segment = (Int, Int)
type Point = Int
data PointType = LPoint | Point | RPoint deriving (Show, Eq, Ord)
type SPPair = (Int, PointType)

parseIntegers :: String -> [Int]
parseIntegers = map read . words

parseSegments :: [String] -> [Segment]
parseSegments = map parseSegment

parseSegment :: String -> Segment
parseSegment segmentStr =
    let [x, y] = map read . words $ segmentStr
     in if x < y then (x, y) else (y, x)

toSPPairs :: [Segment] -> [Point] -> [SPPair]
toSPPairs segments points =
    let lSegments = map (\(x, _) -> (x, LPoint)) segments
        rSegments = map (\(_, x) -> (x, RPoint)) segments
        pPoints = map (\x -> (x, Point)) points
     in sort (lSegments ++ rSegments ++ pPoints)

countSegment :: Int -> SPPair -> Int
countSegment n (_, l)
  | l == LPoint = n + 1
  | l == RPoint = n -1
  | otherwise = n

binarySearch :: Arr SPPair -> SPPair -> Int
binarySearch xs = recBinarySearch (start + 1, end + 1) xs
    where (start, end) = A.bounds xs

recBinarySearch :: (Int, Int) -> Arr SPPair -> SPPair -> Int
recBinarySearch (start, end) xs y
  | end < start = -1
  | mid == y = n - 1
  | otherwise = recBinarySearch (newStart, newEnd) xs y
  where n = div (end + start) 2
        mid = xs A.! (n - 1)
        (newStart, newEnd) = if mid < y then (n + 1, end) else (start, n - 1)

{------------------------------------------------------------------------------}

toArray :: [a] -> Arr a
toArray xs = A.listArray (0, length xs - 1) xs
