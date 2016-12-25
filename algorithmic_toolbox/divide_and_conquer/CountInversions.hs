import qualified Data.Sequence as S

main :: IO ()
main = do
    _ <- getLine
    xs <- fmap parseIntegers getLine
    let (_, numInversions) = mergeSort xs
    print numInversions

type Vector = S.Seq Int

parseIntegers :: String -> Vector
parseIntegers = S.fromList . map read . words

mergeSort :: Vector -> (Vector, Integer)
mergeSort xs
  | S.length xs == 1 = (xs, 0)
  | otherwise =
      let splitIx = div (S.length xs) 2
          (sortedLeft, numInvsLeft) = mergeSort (S.take splitIx xs)
          (sortedRight, numInvsRight) = mergeSort (S.drop splitIx xs)
          (sortedMerged, numInvsMerge) = merge sortedLeft sortedRight
       in (sortedMerged, numInvsLeft + numInvsRight + numInvsMerge)

merge :: Vector -> Vector -> (Vector, Integer)
merge xs ys
  | S.null xs = (ys, 0)
  | S.null ys = (xs, 0)
  | otherwise =
      let (x, y) = (S.index xs 0, S.index ys 0)
          xLength = fromIntegral (S.length xs)
       in if x <= y
             then let (mergedTail, numInvs) = merge (S.drop 1 xs) ys
                   in (x S.<| mergedTail, numInvs)
             else let (mergedTail, numInvs) = merge xs (S.drop 1 ys)
                   in (y S.<| mergedTail, numInvs + xLength)
