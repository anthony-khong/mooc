import qualified Data.Sequence as S

type Arr a = S.Seq a
type Value = Int

last' :: Arr a -> a
last' xs = S.index xs (S.length xs - 1)

{---------------------------------------------------------------------}

main :: IO ()
main = do
    xs <- getLine
    ys <- getLine
    print . last' . last $ minEditDist xs ys

minEditDist :: String -> String -> [Arr Value]
minEditDist xs ys = scanl nextChar initValues (zip [1..] ys)
    where nextChar = minEditDistFixed xs
          initValues = S.fromList [0..length xs]

minEditDistFixed :: String -> Arr Value -> (Int, Char) -> Arr Value
minEditDistFixed xs lastVs (n, y) = foldl nextChar initValue xs
    where nextChar = nextEditDist lastVs y
          initValue = S.empty S.|> n

nextEditDist :: Arr Value -> Char -> Arr Value -> Char -> Arr Value
nextEditDist lastVs y vs x =
    let n = S.length vs
        ins = S.index vs (n - 1) + 1
        del = S.index lastVs n + 1
        lastDiag = S.index lastVs (n - 1)
        mat = if x == y then lastDiag else lastDiag + 1
     in vs S.|> minimum [ins, del, mat]
