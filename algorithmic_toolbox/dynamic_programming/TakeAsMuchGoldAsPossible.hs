import qualified Data.Sequence as S

type Arr a = S.Seq a
type Capacity = Int
type Weight = Int

last' :: Arr a -> a
last' xs = S.index xs (S.length xs - 1)

{---------------------------------------------------------------------}

main :: IO ()
main = do
    [capacity, _] <- fmap parseInts getLine
    weights <- fmap parseInts getLine
    print (solve capacity weights)

parseInts :: String -> [Int]
parseInts = map read . words

solve :: Capacity -> [Weight] -> Weight
solve capacity weights = last' . last $ (maxWeights capacity weights)

maxWeights :: Capacity -> [Weight] -> [Arr Weight]
maxWeights capacity weights = scanl nextItem valuesInit weights
    where valuesInit = S.replicate capacity 0
          nextItem = maxByItem capacity

maxByItem :: Capacity -> Arr Weight -> Weight -> Arr Weight
maxByItem capacity lastVs w = foldl nextCap S.empty [1..capacity]
    where nextCap = maxByCapGivenItem lastVs w

maxByCapGivenItem :: Arr Weight -> Weight -> Arr Weight -> Weight -> Arr Weight
maxByCapGivenItem lastVs w vs c =
    let weightUnused = S.index lastVs (c - 1)
        weightUsed = case (compare c w) of
                       LT -> 0
                       EQ -> w
                       GT -> S.index lastVs (c - w - 1) + w
     in vs S.|> max weightUsed weightUnused
