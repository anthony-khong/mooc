import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array       as A
import qualified Data.Array.ST    as STA
import           Data.List

type Arr a = A.Array Int a
type BSTArr s a = ST s (STA.STArray s Int a)
type STArr s a = STA.STArray s Int a

{-TODO: check correctness-}
{-TODO: swap ST Array to IO Array-}

main :: IO ()
main = do
    _ <- getLine
    xs <- getInts
    let heap = buildHeap xs
    print heap

getInts :: IO [Int]
getInts = getLine >>= return . map read . words

buildHeap :: [Int] -> Arr Int
buildHeap xs = STA.runSTArray (buildHeap' arr initIter)
    where arr = toSTArr . toArray $ xs
          initIter = (length xs) `div` 2

buildHeap' :: BSTArr s Int -> Int -> BSTArr s Int
buildHeap' xs i =
    if i == 1
       then xs
       else buildHeap' (siftDown i xs) (i - 1)

oneToZero :: (Int -> Int) -> (Int -> Int)
oneToZero f = subtract 1 . f . (+ 1)

left :: Int -> Int
left = oneToZero (*2)

right :: Int -> Int
right = oneToZero $ (+1) . (*2)

siftDown :: Int -> BSTArr s Int -> BSTArr s Int
siftDown i xs = do
    j <- xs >>= getSwapIx [i, left i, right i]
    swapped <- xs >>= swapIndex i j
    return swapped

getSwapIx :: [Int] -> STArr s Int -> ST s Int
getSwapIx ixs xs = do
    n <- STA.getBounds xs >>= boundsToLength
    vs <- mapM (safeRead xs n) ixs
    return (swapIx vs)

safeRead :: STArr s Int -> Int -> Int -> ST s Int
safeRead xs n i = if i < n then STA.readArray xs i else return minBound

swapIx :: [Int] -> Int
swapIx xs = snd . head . sort $ zip xs [0..]

swapIndex :: Int -> Int -> STArr s Int -> BSTArr s Int
swapIndex i j xs = do
    [xi, xj] <- mapM (STA.readArray xs) [i, j]
    STA.writeArray xs i xj
    STA.writeArray xs j xi
    return xs

toSTArr :: Arr Int -> BSTArr s Int
toSTArr xs = do
    n <- boundsToLength (A.bounds xs)
    xs' <- STA.newArray (0, n-1) 0
    forM_ [0..(n-1)] $ \i -> STA.writeArray xs' i (xs A.! i)
    return xs'

toArray :: [a] -> Arr a
toArray xs = A.listArray (0, length xs - 1) xs

boundsToLength :: Monad m => (Int, Int) -> m Int
boundsToLength = return . (+1) . negate . uncurry (-)
