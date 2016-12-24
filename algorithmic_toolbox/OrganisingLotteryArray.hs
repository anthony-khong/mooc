import           Control.Monad
import qualified Data.Array    as A
import           Data.List

type Arr a = A.Array Int a

main :: IO ()
main = do
    [nSegments, _] <- fmap parseIntegers getLine
    segments <- fmap (toArray . parseSegments) (replicateM nSegments getLine)
    points <- fmap (toArray . parseIntegers) getLine
    let nContained = fmap (nSegmentsContained segments) points
    putStrLn . unwords . A.elems . fmap show $ nContained

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

nSegmentsContained :: Arr Segment -> Point -> Int
nSegmentsContained ss p = length . filter highOkay . A.elems . take' n $ ss
    where n = nLowOkay ss p
          highOkay (_, high) = high >= p

{-TODO: binary search-}

{------------------------------------------------------------------------------}

toArray :: [a] -> Arr a
toArray xs = A.listArray (0, length xs - 1) xs

length' :: Arr a -> Int
length' = (+1) . negate . uncurry (-) . A.bounds

take' :: Int -> Arr a -> Arr a
take' n xs = A.ixmap (0, m - 1) id xs
    where m = min n (length' xs)

head' :: Arr a -> a
head' xs = xs A.! 0
