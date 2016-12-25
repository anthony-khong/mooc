main :: IO ()
main = do
    target <- getLine >>= readInteger
    let prizes = maxNumPrizePlaces target
    print (length prizes)
    putStrLn . unwords . map show $  prizes

readInteger :: String -> IO Integer
readInteger = return . read

maxNumPrizePlaces :: Integer -> [Integer]
maxNumPrizePlaces 2 = [2]
maxNumPrizePlaces target =
    let minInt = invSumInt target
        partialSum = sumInt minInt
        remainder = target - partialSum
     in if remainder <= minInt then [1..(minInt - 1)] ++ [minInt + remainder] else [1..minInt] ++ [remainder]

invSumInt :: Integer -> Integer
invSumInt n = floorSqrt (fromIntegral (2*n) + 1) - 1

floorSqrt :: Double -> Integer
floorSqrt = floor . sqrt

sumInt :: Integer -> Integer
sumInt n = div (n * (n + 1)) 2

