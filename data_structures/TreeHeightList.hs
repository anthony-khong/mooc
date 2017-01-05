import           Data.List
import           Data.Ord  (comparing)

main :: IO ()
main = do
    _ <- getLine
    xs <- fmap parseInts getLine
    let tree = fromList xs
    print (height tree)

parseInts :: String -> [Int]
parseInts = map read . words

data Tree a = Node a [Tree a] deriving (Show)
type IxLookUpTable = (Int, [[Int]])

fromList :: [Int] -> Tree Int
fromList xs = fromList' table
    where table = ixsToTable xs

fromList' :: IxLookUpTable -> Tree Int
fromList' (root, ts) = subtree root
    where subtree x = Node x (map subtree (ts !! x))

height :: Tree a -> Int
height (Node _ []) = 1
height (Node _ ts) = 1 + maximum (map height ts)

groupIxs :: [Int] -> [(Int, [Int])]
groupIxs ixs = map collapseFst . groupByFst . sortByFst $ zip ixs [0..]
    where compareFst (x, _) (y, _) = x == y
          groupByFst = groupBy compareFst
          sortByFst = sortBy (comparing fst)
          collapseFst xs = (fst (head xs), map snd xs)

toTable :: [(Int, [Int])] -> IxLookUpTable
toTable groupedIxs = (rootIx, reverse $ foldl buildTable [] (tail groupedIxs))
    where [rootIx] = snd . head $ groupedIxs
          buildTable ts (ix, ixs) = if length ts == ix
                                       then ixs : ts
                                       else ixs : replicate (ix - length ts) [] ++ ts

ixsToTable :: [Int] -> IxLookUpTable
ixsToTable ixs = (rootIx, ts ++ replicate nPads [])
    where (rootIx, ts) = toTable . groupIxs $ ixs
          nPads = length ixs - length ts


{-
ixs = [4,-1,4,1,1]
10
ixs = [8, 8, 5, 6, 7, 3, 1, 6, -1, 5]
-}
