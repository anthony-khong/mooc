import           Data.List

main :: IO ()
main = do
    _ <- getLine
    integers <- fmap words getLine
    putStrLn (maxSalary integers)

maxSalary :: [String] -> String
maxSalary integers =
    let sortedIntegers = sortBy (flip compareIntString) integers
     in foldl1 (++) sortedIntegers

compareIntString :: String -> String -> Ordering
compareIntString xs ys = compare (xs ++ ys) (ys ++ xs)
