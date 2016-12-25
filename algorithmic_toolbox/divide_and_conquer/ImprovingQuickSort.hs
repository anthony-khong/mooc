main :: IO ()
main = do
    _ <- getLine
    xs <- fmap parseIntegers getLine
    let sorted = quickSort xs
    putStrLn . unwords . fmap show $ sorted

parseIntegers :: String -> [Int]
parseIntegers = map read . words

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let lessThanX = filter (< x) xs
        equalToX = x : filter (== x) xs
        moreThanX = filter (> x) xs
     in quickSort lessThanX ++ equalToX ++ quickSort moreThanX
