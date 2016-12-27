import qualified Data.Sequence as S

type Arr a = S.Seq a
type TwoTable a = Arr (Arr a)
type ThreeTable a = Arr (TwoTable a)
type Value = Int

last' :: Arr a -> a
last' xs = S.index xs (S.length xs - 1)

{---------------------------------------------------------------------}

main :: IO ()
main = do
    _ <- getLine
    xs <- fmap parseInts getLine
    _ <- getLine
    ys <- fmap parseInts getLine
    _ <- getLine
    zs <- fmap parseInts getLine
    print . last' . last' . last' $ longestSubSeq3 xs ys zs

parseInts :: String -> [Int]
parseInts = map read . words

longestSubSeq3 :: [Int] -> [Int] -> [Int] -> ThreeTable Value
longestSubSeq3 xs ys = foldl (longestSubSeq2 xs ys) initValues
    where (lxs, lys) = (length xs, length ys)
          initValues = S.empty S.|> S.replicate (lys + 1) (S.replicate (lxs + 1) 0)

longestSubSeq2 :: [Int] -> [Int] -> ThreeTable Value -> Int -> ThreeTable Value
longestSubSeq2 xs ys last3Vs z =  last3Vs S.|> foldl longestSubSeq1' initValue ys
    where initValue = S.empty S.|> S.replicate (length xs + 1) 0
          longestSubSeq1' = longestSubSeq1 last3Vs xs z

longestSubSeq1 :: ThreeTable Value -> [Int] -> Int -> TwoTable Value -> Int -> TwoTable Value
longestSubSeq1 last3Vs xs z last2Vs y = last2Vs S.|> foldl nextDigit' initValue xs
    where initValue = S.fromList [0]
          nextDigit' = nextDigit last3Vs y last2Vs z

nextDigit :: ThreeTable Value -> Int -> TwoTable Value -> Int -> Arr Value -> Int -> Arr Value
nextDigit last3Vs y last2Vs z vs x =
    let (i, j) = (S.length vs, S.length last2Vs)
        delX = last' vs
        delY = S.index (last' last2Vs) i
        delZ = S.index (S.index (last' last3Vs) j) i
        swap = S.index (S.index (last' last3Vs) (j - 1)) (i - 1)
        match = if (x == y) && (y == z) then swap + 1 else swap
     in vs S.|> maximum [delX, delY, delZ, match]

{-

:l LCS3.hs

xs = [8, 3, 2, 1, 7]
ys = [8, 2, 1, 3, 8, 10, 7]
zs = [6, 8, 3, 1, 4, 7]
longestSubSeq3 xs ys zs


longestSubSeq :: [Int] -> [Int] -> Arr (Arr Value)
longestSubSeq xs = foldl (longestSubSeqOver xs) initValues
    where initValues = S.empty S.|> S.replicate (length xs + 1) 0

longestSubSeqOver :: [Int] -> Arr (Arr Value) -> Int -> Arr (Arr Value)
longestSubSeqOver xs lastVs y = lastVs S.|> foldl nextDigit' initValue xs
    where initValue = S.fromList [0]
          nextDigit' = nextDigit lastVs y

nextDigit :: Arr (Arr Value) -> Int -> Arr Value -> Int -> Arr Value
nextDigit lastVs y vs x =
    let i = S.length vs
        ins = S.index (last' lastVs) i
        del = S.index vs (i - 1)
        swap = S.index (last' lastVs) (i - 1)
        match = if x == y then swap + 1 else swap
     in vs S.|> maximum [ins, del, match]
-}
