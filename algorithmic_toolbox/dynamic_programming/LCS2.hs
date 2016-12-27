import qualified Data.Sequence as S

type Arr a = S.Seq a
type Value = Int

last' :: Arr a -> a
last' xs = S.index xs (S.length xs - 1)

{---------------------------------------------------------------------}

main :: IO ()
main = do
    xs <- fmap parseInts getLine
    ys <- fmap parseInts getLine
    {-print . last' . last' $ longestSubSeq xs ys-}
    print (longestSubSeq xs ys)

parseInts :: String -> [Int]
parseInts = map read . words

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

{-
longestSubSeq xs ys

xs = [8, 2, 1, 3, 8, 10, 7]
ys = [6, 8, 3, 1, 4, 7]
lastVs = S.empty S.|> S.replicate (length xs) 0
(x, y) = (xs !! 2, ys !! 0)
vs = S.fromList [0, 0, 1]

-}
