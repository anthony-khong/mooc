import           Data.List

main :: IO ()
main = do
    _ <- getLine
    revenues <- getLine >>= parseNumbers
    clicks <- getLine >>= parseNumbers
    print (maxAdRevenue revenues clicks)

type Revenue = Integer
type Click = Integer

parseNumbers :: String -> IO [Integer]
parseNumbers = return . map read . words

maxAdRevenue :: [Revenue] -> [Click] -> Revenue
maxAdRevenue revenues clicks =
    let sortedRevenues = sortBy (flip compare) revenues
        sortedClicks = sortBy (flip compare) clicks
     in sum $ zipWith (*) sortedRevenues sortedClicks
