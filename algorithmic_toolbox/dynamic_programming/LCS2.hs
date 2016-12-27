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
    print . last' . last' $ longestSubSeq xs ys

longestSubSeq :: [Int] -> [Int] -> Arr (Arr Value)
longestSubSeq = undefined






