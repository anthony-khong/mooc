main :: IO()
main = do
    strOfInts <- getLine
    let [a, b] = map read . words $ strOfInts :: [Integer]
    print (eulerGCD a b)

naiveGCD :: Integer -> Integer -> Integer
naiveGCD a b = last [x | x <- [1..m], mod a x == 0, mod b x == 0]
    where m = max a b

eulerGCD :: Integer -> Integer -> Integer
eulerGCD a 0 = a
eulerGCD a b = eulerGCD b (mod a b)
