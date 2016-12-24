import           Data.List

main :: IO ()
main = do
    _ <- getLine
    strOfInts <- getLine
    let integers = map read . words $ strOfInts :: [Integer]
    print (fastMaxPairwiseProduct integers)

fastMaxPairwiseProduct :: (Num a, Ord a) => [a] -> a
fastMaxPairwiseProduct xs =
    let sorted = sortBy (flip compare) xs
        largest = head sorted
        secondLargest = sorted !! 1
     in largest * secondLargest

maxPairwiseProduct :: (Num a, Ord a) => [a] -> a
maxPairwiseProduct = maxProduct . pairs

pairs :: [a] -> [(a, a)]
pairs list = [(x, y) | (x:ys) <- tails list, y <- ys]

maxProduct :: (Num a, Ord a) => [(a, a)] -> a
maxProduct = maximum . map (uncurry (*))
