import           System.Random

main :: IO ()
main = print ()

prime :: Integer
prime = 100000007

randInt :: IO Integer
randInt = do
    gen <- getStdGen
    let (output, _) = randomR (1, prime - 1) gen
    return output

rabinKarp :: Int -> String -> String -> [Int]
rabinKarp x txt ptrn = foldl checkResult [] (hashes x txt ptrn)
    where checkResult = undefined

hashes :: Int -> String -> String -> [Int]
hashes x txt ptrn =
    let adj = adjustmentFactor (length ptrn) x
        initStr = drop (length ptrn - length txt) txt
     in undefined

adjustmentFactor :: Int -> Int -> Int
adjustmentFactor len x = iterate (\y -> y*x `mod` len) 1 !! len


