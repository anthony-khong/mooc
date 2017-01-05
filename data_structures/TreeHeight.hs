import           Control.Monad
import qualified Data.Array    as A
import qualified Data.Array.ST as STA
import           Data.List
import qualified Data.Sequence as S

type Arr a = A.Array Int a
type Seq a = S.Seq a
type Table = Arr (Seq Int)
data Tree a = Node a (Seq (Tree a)) deriving (Show)

main :: IO ()
main = do
    _ <- getLine
    xs <- fmap parseInts getLine
    let tree = fromList xs
    print (height tree)

parseInts :: String -> [Int]
parseInts = map read . words

toArray :: [a] -> Arr a
toArray xs = A.listArray (0, length xs - 1) xs

aLength :: Arr a -> Int
aLength = (+1) . negate . uncurry (-) . A.bounds

sMaximum :: (Ord a, Bounded a) => Seq a -> a
sMaximum = S.foldlWithIndex (\x _ y -> max x y) minBound

fromList :: [Int] -> Tree Int
fromList xs = fromList' root table
    where table = makeLookUpTable . toArray $ xs
          (Just root) = elemIndex (-1) xs

fromList' :: Int -> Table -> Tree Int
fromList' root table = subtree root
    where subtree x = Node x (fmap subtree (table A.! x))

height :: Tree a -> Int
height (Node _ ts)
  | S.null ts = 1
  | otherwise = 1 + sMaximum (fmap height ts)

makeLookUpTable :: Arr Int -> Table
makeLookUpTable xs = STA.runSTArray $ do
    let n = aLength xs
    table <- STA.newArray (0, n-1) S.empty
    forM_ [0..(n-1)] $ \i ->
        if xs A.! i == (-1)
           then return ()
           else do
               let parentIx = xs A.! i
               children <- STA.readArray table parentIx
               STA.writeArray table parentIx (children S.|> i)
    return table

{-
xs = [4,-1,4,1,1]
xs = [8, 8, 5, 6, 7, 3, 1, 6, -1, 5]
-}
