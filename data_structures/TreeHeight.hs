import           Control.Monad.ST
import qualified Data.Array       as A
import qualified Data.Array.ST    as STA
import           Data.List
import qualified Data.Sequence    as S

main :: IO ()
main = do
    _ <- getLine
    xs <- fmap parseInts getLine
    let tree = fromList xs
    print (height tree)

parseInts :: String -> [Int]
parseInts = map read . words

type Arr a = A.Array Int a
type Seq a = S.Seq a
type Table = Arr (Seq Int)
type MutableTable s = ST s (STA.STArray s Int (Seq Int))
data Tree a = Node a (Seq (Tree a)) deriving (Show)

fromList :: [Int] -> Tree Int
fromList xs = fromList' root table
    where table = makeLookUpTable xs
          (Just root) = elemIndex (-1) xs

fromList' :: Int -> Table -> Tree Int
fromList' root table = subtree root
    where subtree x = Node x (fmap subtree (table A.! x))

height :: Tree a -> Int
height (Node _ ts)
  | S.null ts = 1
  | otherwise = 1 + maximum (fmap height ts)

makeLookUpTable :: [Int] -> Table
makeLookUpTable xs = STA.runSTArray $ foldl addGroup initTable zipped
    where initTable = emptyTable (length xs)
          zipped = zip [0..] xs

emptyTable :: Int -> MutableTable s
emptyTable n = STA.newArray (0, n - 1) S.empty

addGroup :: MutableTable s -> (Int, Int) -> MutableTable s
addGroup xs (childIx, parentIx) = do
    xs' <- xs
    return xs'

newArr = STA.runSTArray $ do
    xs <- STA.newArray (0, 10) 3 :: ST s (STA.STArray s Int Int)
    return xs

{-
ixs = [4,-1,4,1,1]
ixs = [8, 8, 5, 6, 7, 3, 1, 6, -1, 5]
-}
