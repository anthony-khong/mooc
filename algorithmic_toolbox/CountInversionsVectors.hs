import qualified Data.Vector as V

main :: IO ()
main = do
    _ <- getLine
    xs <- fmap parseIntegers getLine
    let (_, numInversions) = mergeSort xs
    print numInversions

type Vector = V.Vector Int

parseIntegers :: String -> Vector
parseIntegers = V.fromList . map read . words

mergeSort :: Vector -> (Vector, Integer)
mergeSort xs
  | V.length xs == 1 = (xs, 0)
  | otherwise =
      let splitIx = div (V.length xs) 2
          (sortedLeft, numInvsLeft) = mergeSort (V.take splitIx xs)
          (sortedRight, numInvsRight) = mergeSort (V.drop splitIx xs)
          (sortedMerged, numInvsMerge) = merge sortedLeft sortedRight
       in (sortedMerged, numInvsLeft + numInvsRight + numInvsMerge)

merge :: Vector -> Vector -> (Vector, Integer)
merge xs ys
  | V.null xs = (ys, 0)
  | V.null ys = (xs, 0)
  | otherwise =
      let (x, y) = (V.head xs, V.head ys)
          xLength = fromIntegral (V.length xs)
       in if x <= y
             then let (mergedTail, numInvs) = merge (V.tail xs) ys
                   in (V.cons x mergedTail, numInvs)
             else let (mergedTail, numInvs) = merge xs (V.tail ys)
                   in (V.cons y mergedTail, numInvs + xLength)
