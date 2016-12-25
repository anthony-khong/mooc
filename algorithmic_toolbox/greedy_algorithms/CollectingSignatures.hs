import           Control.Monad
import           Data.List

main :: IO ()
main = do
    numSegments <- getLine >>= readInt
    segments <- replicateM numSegments getLine >>= readSegments
    let visits = minVisit segments
    print (length visits)
    mapM_ print visits

type Segment = (Integer, Integer)
type MarkedPoint = Integer

readInt :: String -> IO Int
readInt = return . read

readSegments :: [String] -> IO [Segment]
readSegments = return . map readOneSegment

readOneSegment :: String -> Segment
readOneSegment line =
    let [x, y] = (map read . words $ line) :: [Integer]
     in (x, y)

minVisit :: [Segment] -> [MarkedPoint]
minVisit segments =
    let sortedSegments = sortBy (\x y -> compare (snd x) (snd y)) segments
     in foldl markPoint [] sortedSegments

markPoint :: [MarkedPoint] -> Segment -> [MarkedPoint]
markPoint [] (_, end)     = [end]
markPoint points (start, end) = if lastIsInSegment then points else points ++ [end]
    where lastPoint = last points
          lastIsInSegment = (lastPoint >= start) && (lastPoint <= end)
