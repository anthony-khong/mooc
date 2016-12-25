import           Control.Monad
import qualified MaxPairwiseProduct as MPP
import           System.Random

main :: IO ()
main = do
    g <- newStdGen
    let (n, g') = sampleNumInts g
        (integers, _) = sampleIntegers n g'
        result = MPP.maxPairwiseProduct integers
        fastResult = MPP.fastMaxPairwiseProduct integers
    printTestResult n integers result fastResult
    when (result == fastResult) main

printTestResult :: Int -> [Integer] -> Integer -> Integer -> IO ()
printTestResult n integers result fastResult = do
    putStrLn (show n ++ ": " ++ show integers)
    if result == fastResult
       then putStrLn "OK:"
       else putStrLn "Not OK:"
    putStrLn (show result ++ " & " ++ show fastResult ++ ".\n")


randomPos :: (Random a, Num a, RandomGen g) => g -> (a, g)
randomPos g = let (x, g') = random g in (abs x, g')

sampleNumInts :: (Random a, Integral a, RandomGen g) => g -> (a, g)
sampleNumInts g = let (x, g') = randomPos g in (mod x 100 + 2, g')

sampleIntegers :: (RandomGen g) => Int -> g -> ([Integer], g)
sampleIntegers numInts initGen =
    let initIter = randomPos initGen
        n = fromIntegral numInts
        randomIter = take n $ iterate (\(_, g) -> randomPos g) initIter
        integers = map fst randomIter
        newGen = snd $ last randomIter
    in (integers, newGen)

