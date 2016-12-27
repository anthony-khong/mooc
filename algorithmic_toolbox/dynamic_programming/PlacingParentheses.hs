type Op = Int -> Int -> Int
type Exp = ([Int], [Op])
type MinMax = (Int, Int)
type ValueIx = (Int, Int)

main :: IO ()
main = do
    expression <- fmap parseExp getLine
    print . extractMax . solve $ expression

parseExp :: String -> Exp
parseExp = reverseExp . foldl parseSymbol ([], [])
    where reverseExp (x, y) = (reverse x, reverse y)

parseSymbol :: Exp -> Char -> Exp
parseSymbol (digits, operators) symbol
  | symbol == '+' = (digits, (+):operators)
  | symbol == '-' = (digits, (-):operators)
  | symbol == '*' = (digits, (*):operators)
  | otherwise = ((read [symbol]):digits, operators)

extractMax :: [[MinMax]] -> Int
extractMax = snd . last . last

solve :: Exp -> [[MinMax]]
solve xs = foldl (minMaxValues xs) initMinMaxes [1..(nDigits-1)]
    where digits = fst xs
          nDigits = length digits
          initMinMaxes = [zip digits digits]

minMaxValues :: Exp -> [[MinMax]] -> Int -> [[MinMax]]
minMaxValues xs vs row =
    let nDigits = length (fst xs)
        nextMinMaxes = map (minMaxValue xs vs) (subExpIxs row nDigits)
     in vs ++ [nextMinMaxes]

subExpIxs :: Int -> Int -> [[(ValueIx, ValueIx)]]
subExpIxs row nDigits = map expandIxs initIxs
    where nNodesInRow = nDigits - row
          initIxs = [(row, i) | i <- [0..(nNodesInRow-1)]]

expandIxs :: ValueIx -> [(ValueIx, ValueIx)]
expandIxs (i, j) =
    let nextLeft (x, y) = (x - 1, y)
        nextRight (x, y) = (x - 1, y + 1)
        leftIxs = reverse . take i . iterate nextLeft $ nextLeft (i, j)
        rightIxs = take i . iterate nextRight $ nextRight (i, j)
     in zip leftIxs rightIxs

minMaxValue :: Exp -> [[MinMax]] -> [(ValueIx, ValueIx)] -> MinMax
minMaxValue xs vs ixs =
    let (mins, maxs) = unzip (slideMinMax xs vs ixs)
     in (minimum mins, maximum maxs)

slideMinMax :: Exp -> [[MinMax]] -> [(ValueIx, ValueIx)] -> [MinMax]
slideMinMax _ _ []         = []
slideMinMax xs vs (ix:ixs) =
    let ((i, j), (i', j')) = ix
        lSubEx = pairToList . index vs $ (i, j)
        rSubEx = pairToList . index vs $ (i', j')
        operator = (snd xs) !! (i + j)
        allValues = [operator l r | l <- lSubEx, r <- rSubEx]
     in (minimum allValues, maximum allValues) : (slideMinMax xs vs ixs)

index :: [[MinMax]] -> ValueIx -> MinMax
index vs (i, j) = (vs !! i) !! j

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]
