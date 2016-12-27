import qualified Data.Array as A

type Arr a = A.Array Int a
type Action = Int -> Int

toArray :: [a] -> Arr a
toArray xs = A.listArray (0, length xs - 1) xs

{-*Main> solve 10-}
{-[0,1,1,2,3,2,3,3,2,3]-}

{---------------------------------------------------------------------}

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    let solutions = solve n
    print solutions

solve :: Int -> Arr Int
solve n = foldl recSolve emptyArray [1..n]
    where emptyArray = toArray (replicate n maxBound)

recSolve :: Arr Int -> Int -> Arr Int
recSolve vs 1 = vs A.// [(0, 0)]
recSolve vs n = vs A.// [(n - 1, solution)]
    where invOperations = [divTwo, divThree, minOne]
          possibleValues = map (nextValue vs n) invOperations
          solution = minimum possibleValues

nextValue :: Arr Int -> Int -> Action -> Int
nextValue vs n f = lookupValue vs (f n) + 1

lookupValue :: Arr Int -> Int -> Int
lookupValue vs n = if n < 0 then (maxBound - 1) else vs A.! (n - 1)

safeDivBy :: Int -> Int -> Int
safeDivBy m n = if n `mod` m == 0 then n `div` m else (-1)

divTwo :: Action
divTwo = safeDivBy 2

divThree :: Action
divThree = safeDivBy 3

minOne :: Action
minOne n = (-) n 1
