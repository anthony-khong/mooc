main :: IO ()
main = do
    n <- getLine
    print . lastSumFib . read $ n

lastSumFib :: Integer -> Integer
lastSumFib n = if lastNextTwoFib == 0 then 9 else lastNextTwoFib - 1
    where lastNextTwoFib = lastFib (n + 2)

lastFib :: Integer -> Integer
lastFib n = mod (fib remainder) 10
    where fib n' = fibs !! fromIntegral n'
          remainder = mod n pisanoTen

fibs :: [Integer]
fibs = map fst $ iterate (\(x, y) -> (y, x + y)) (0, 1)

pisanoTen :: Integer
pisanoTen = 60
