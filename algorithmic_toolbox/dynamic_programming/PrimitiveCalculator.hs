import qualified Data.Sequence as S

type Arr a = S.Seq a
type Action = Int -> Int

last' :: Arr a -> a
last' xs = S.index xs (S.length xs - 1)

{---------------------------------------------------------------------}

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    let solutions = solve n
        steps = backtrack solutions n
    print (last' solutions)
    putStrLn . unwords . map show . reverse $ steps

solve :: Int -> Arr Int
solve n = foldl recSolve S.empty [1..n]

recSolve :: Arr Int -> Int -> Arr Int
recSolve vs 1 = vs S.|> 0
recSolve vs n = vs S.|> solution
    where invOperations = [divTwo, divThree, minOne]
          possibleValues = map (nextValue vs n) invOperations
          solution = minimum possibleValues

nextValue :: Arr Int -> Int -> Action -> Int
nextValue vs n f = lookupValue vs (f n) + 1

lookupValue :: Arr Int -> Int -> Int
lookupValue vs n = if n < 0 then (maxBound - 1) else S.index vs (n - 1)

safeDivBy :: Int -> Int -> Int
safeDivBy m n = if n `mod` m == 0 then n `div` m else (-1)

divTwo :: Action
divTwo = safeDivBy 2

divThree :: Action
divThree = safeDivBy 3

minOne :: Action
minOne n = (-) n 1

backtrack :: Arr Int -> Int -> [Int]
backtrack vs n
  | n < 1 = []
  | otherwise = n : (backtrack vs prevSolution)
      where prevSolution = stepBack vs n

stepBack :: Arr Int -> Int -> Int
stepBack vs n
    | nextValue vs n divTwo == solution = divTwo n
    | nextValue vs n divThree == solution = divThree n
    | otherwise = minOne n
    where solution = S.index vs (n - 1)
