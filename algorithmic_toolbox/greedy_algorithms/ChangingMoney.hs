main :: IO ()
main = do
    n <- getLine
    print . minNumCoins . read $ n

minNumCoins :: Integer -> Integer
minNumCoins n =
    let (numTens, leftoverTen) = divMod n 10
        (numFives, leftoverFive) = divMod leftoverTen 5
        numOnes = leftoverFive
     in numTens + numFives + numOnes
