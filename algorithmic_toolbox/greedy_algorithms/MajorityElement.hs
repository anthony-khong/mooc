import           Data.List

main :: IO ()
main =  do
    _ <- getLine
    xs <- fmap parseIntegers getLine
    let maxFreq = maxFrequency xs
        output = if maxFreq > 0.5 then 1 else 0
    print (output :: Int)

parseIntegers :: String -> [Int]
parseIntegers = map read . words

maxFrequency :: Ord a => [a] -> Float
maxFrequency xs = maxLength / n
    where grouped = group (sort xs)
          lengths = map length grouped
          maxLength = fromIntegral (maximum lengths)
          n = fromIntegral (length xs)
