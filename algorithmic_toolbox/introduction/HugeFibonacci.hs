main :: IO ()
main = do
    strOfInts <- getLine
    let [a, b] = map read . words $ strOfInts :: [Integer]
    print (hugeFibModM a b)

hugeFibModM :: Integer -> Integer -> Integer
hugeFibModM n m = mod (fibIter remainder) m
    where period = pisanoPeriod m
          remainder = mod n period

fibIter :: Integer -> Integer
fibIter n = fibs !! fromIntegral n

fibs :: [Integer]
fibs = map fst $ iterate (\(x, y) -> (y, x + y)) (0, 1)

fibsMod :: Integer -> [Integer]
fibsMod m = map fst $ iterate (\(x, y) -> (y, mod (x + y) m)) (0, 1)

pisanoPeriod :: Integer -> Integer
pisanoPeriod m = fromIntegral period
    where fibsModM = fibsMod m
          fibsPair = tail $ zip fibsModM (tail fibsModM)
          endOfNewPeriod (x, y) = (x /= 0) || (y /= 1)
          period = 1 + length (takeWhile endOfNewPeriod fibsPair)
