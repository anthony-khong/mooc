main :: IO()
main = do
    strOfInts <- getLine
    let [a, b] = map read . words $ strOfInts :: [Integer]
    print (eulerLCM a b)

eulerLCM :: Integer -> Integer -> Integer
eulerLCM a b = div (a * b) (eulerGCD a b)

eulerGCD :: Integer -> Integer -> Integer
eulerGCD a 0 = a
eulerGCD a b = eulerGCD b (mod a b)
