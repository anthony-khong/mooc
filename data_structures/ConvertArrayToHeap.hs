import           Control.Monad
import qualified Data.Array    as A
import qualified Data.Array.IO as MA
import           Data.Foldable (toList)
import           Data.List
import qualified Data.Sequence as S

type Arr a = A.Array Int a
type Seq a = S.Seq a
type MArr a = MA.IOArray Int a
type HMArr a = IO (MArr a, Seq (Int, Int))

main :: IO ()
main = do
    _ <- getLine
    xs <- getInts
    (_, hs) <- buildHeap xs
    print (S.length hs)
    mapM_ printSwap (toList hs)

getInts :: IO [Int]
getInts = getLine >>= return . map read . words

printSwap :: (Int, Int) -> IO ()
printSwap (x, y) = putStrLn (show x ++ " " ++ show y)

buildHeap :: [Int] -> HMArr Int
buildHeap xs = buildHeap' initHistArr initIter
    where initHistArr = do
            initArr <- toSTArr . toArray $ xs
            return (initArr, S.empty)
          initIter = (length xs `div` 2) + 1

buildHeap' :: HMArr Int -> Int -> HMArr Int
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

siftDown :: Int -> HMArr Int -> HMArr Int
siftDown i hxs = do
    (xs, hs) <- hxs
    j <- (return xs) >>= getSwapIx [i, left i, right i]
    if i == j
       then hxs
       else do
           xs' <- (return xs) >>= swapIndex i j
           let hs' = hs S.|> (i, j)
               hxs' = return (xs', hs')
           siftDown j hxs'

getSwapIx :: [Int] -> MArr Int -> IO Int
getSwapIx ixs xs = do
    n <- MA.getBounds xs >>= boundsToLength
    vs <- mapM (safeRead xs n) ixs
    return (swapIx vs ixs)

safeRead :: MArr Int -> Int -> Int -> IO Int
safeRead xs n i = if i < n then MA.readArray xs i else return maxBound

swapIx :: [Int] -> [Int] -> Int
swapIx xs ixs = snd . head . sort $ zip xs ixs

swapIndex :: Int -> Int -> MArr Int -> IO (MArr Int)
swapIndex i j xs = do
    [xi, xj] <- mapM (MA.readArray xs) [i, j]
    MA.writeArray xs i xj
    MA.writeArray xs j xi
    return xs

toSTArr :: Arr Int -> IO (MArr Int)
toSTArr xs = do
    n <- boundsToLength (A.bounds xs)
    xs' <- MA.newArray (0, n-1) 0
    forM_ [0..(n-1)] $ \i -> MA.writeArray xs' i (xs A.! i)
    return xs'

toArray :: [a] -> Arr a
toArray xs = A.listArray (0, length xs - 1) xs

boundsToLength :: Monad m => (Int, Int) -> m Int
boundsToLength = return . (+1) . negate . uncurry (-)
