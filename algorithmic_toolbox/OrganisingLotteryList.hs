import           Control.Monad
import           Data.List

main :: IO ()
main = do
    [nSegments, _] <- fmap parseIntegers getLine
    segments <- fmap parseSegments (replicateM nSegments getLine)
    points <- fmap parseIntegers getLine
    let nContained = map (nSegmentsContained segments) points
    putStrLn . unwords . map show $ nContained

type Segment = (Int, Int)
type Point = Int

parseIntegers :: String -> [Int]
parseIntegers = map read . words

parseSegments :: [String] -> [Segment]
parseSegments = sortBy compareFirst . map parseSegment
    where compareFirst (x, _) (y, _) = compare x y

parseSegment :: String -> Segment
parseSegment segmentStr =
    let [x, y] = map read . words $ segmentStr
     in if x < y then (x, y) else (y, x)

nSegmentsContained :: [Segment] -> Point -> Int
nSegmentsContained ss p = length . filter highOkay . take n $ ss
    where n = nLowOkay ss p
          highOkay (_, high) = high >= p

nLowOkay :: [Segment] -> Point -> Int
nLowOkay [] _ = 0
nLowOkay ((x,_):ss) p
    | x <= p = 1 + nLowOkay ss p
    | otherwise = 0
