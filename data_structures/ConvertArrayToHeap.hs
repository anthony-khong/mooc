import           Control.Monad
import qualified Data.Array    as A
import qualified Data.Array.IO as MA
import           Data.List

type Arr a = A.Array Int a
type MArr a = MA.IOArray Int a
type BMArr a = IO (MArr a)

main :: IO ()
main = do
    _ <- getLine
    xs <- getInts
    heap <- buildHeap xs
    hs <- mapM (MA.readArray heap) [0..4]
    print hs

getInts :: IO [Int]
getInts = getLine >>= return . map read . words

buildHeap :: [Int] -> BMArr Int
buildHeap xs = buildHeap' arr initIter
    where arr = toSTArr . toArray $ xs
          initIter = (length xs `div` 2) - 1

buildHeap' :: BMArr Int -> Int -> BMArr Int
buildHeap' xs i =
    if i < 0
       then xs
       else buildHeap' (siftDown i xs) (i - 1)

oneToZero :: (Int -> Int) -> (Int -> Int)
oneToZero f = subtract 1 . f . (+ 1)

left :: Int -> Int
left = oneToZero (*2)

right :: Int -> Int
right = oneToZero $ (+1) . (*2)

siftDown :: Int -> BMArr Int -> BMArr Int
siftDown i xs = do
    j <- xs >>= getSwapIx [i, left i, right i]
    if i == j
       then xs
       else siftDown j (xs >>= swapIndex i j)

getSwapIx :: [Int] -> MArr Int -> IO Int
getSwapIx ixs xs = do
    n <- MA.getBounds xs >>= boundsToLength
    vs <- mapM (safeRead xs n) ixs
    return (swapIx vs ixs)

safeRead :: MArr Int -> Int -> Int -> IO Int
safeRead xs n i = if i < n then MA.readArray xs i else return maxBound

swapIx :: [Int] -> [Int] -> Int
swapIx xs ixs = snd . head . sort $ zip xs ixs

swapIndex :: Int -> Int -> MArr Int -> BMArr Int
swapIndex i j xs = do
    [xi, xj] <- mapM (MA.readArray xs) [i, j]
    MA.writeArray xs i xj
    MA.writeArray xs j xi
    return xs

toSTArr :: Arr Int -> BMArr Int
toSTArr xs = do
    n <- boundsToLength (A.bounds xs)
    xs' <- MA.newArray (0, n-1) 0
    forM_ [0..(n-1)] $ \i -> MA.writeArray xs' i (xs A.! i)
    return xs'

toArray :: [a] -> Arr a
toArray xs = A.listArray (0, length xs - 1) xs

boundsToLength :: Monad m => (Int, Int) -> m Int
boundsToLength = return . (+1) . negate . uncurry (-)
