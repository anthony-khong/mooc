main :: IO ()
main = do
    n <- getLine
    print . fibIter . read $ n

fibRec :: Int -> Integer
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n-1) + fibRec (n-2)

fibIter :: Int -> Integer
fibIter n = fst $ fibs !! n
    where fibs = iterate (\(x, y) -> (y, x + y)) (0, 1)

fibTail :: Int -> Integer
fibTail n = fibTailInner 0 1 (fromIntegral n)

fibTailInner :: Integer -> Integer -> Integer -> Integer
fibTailInner prev prevTwo 0 = prev + prevTwo
fibTailInner prev prevTwo n = fibTailInner (prev + prevTwo) prev (n - 1)
