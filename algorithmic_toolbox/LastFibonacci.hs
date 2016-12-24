main :: IO ()
main = do
    n <- getLine
    print . lastFib . read $ n

lastFib :: Int -> Integer
lastFib n = fst $ fibs !! n
    where fibs = iterate (\(x, y) -> (lastDigit y, x + y)) (0, 1)
          lastDigit x = mod x 10
