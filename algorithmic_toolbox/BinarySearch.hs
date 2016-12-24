import           Data.Array

main :: IO ()
main = do
    xs <- fmap (convertToArray . parseIntegers) getLine
    searches <- fmap (convertToArray . parseIntegers) getLine
    let searchResults = fmap (binarySearch xs) searches
    putStrLn . unwords . elems . fmap show $ searchResults

convertToArray :: [a] -> Array Int a
convertToArray xs = listArray (0, length xs - 1) xs

parseIntegers :: String -> [Int]
parseIntegers = drop 1 . map read . words

binarySearch :: Ord a => Array Int a -> a -> Int
binarySearch xs = recBinarySearch (start + 1, end + 1) xs
    where (start, end) = bounds xs

recBinarySearch :: Ord a => (Int, Int) -> Array Int a -> a -> Int
recBinarySearch (start, end) xs y
  | end < start = -1
  | mid == y = n - 1
  | otherwise = recBinarySearch (newStart, newEnd) xs y
  where n = div (end + start) 2
        mid = xs ! (n - 1)
        (newStart, newEnd) = if mid < y then (n + 1, end) else (start, n - 1)

{-
binarySearch :: Ord a => [a] -> a -> Maybe Integer
binarySearch [] _ = Nothing
binarySearch xs y
    | mid == y = Just n'
    | mid < y = fmap (\x -> x + n' + 1) (binarySearch (drop (n + 1) xs) y)
    | otherwise = binarySearch (take n xs) y
  where n = div (length xs) 2
        n' = fromIntegral n
        mid = xs !! n

parseOutput :: Maybe Integer -> Integer
parseOutput Nothing  = -1
parseOutput (Just x) = x
-}


