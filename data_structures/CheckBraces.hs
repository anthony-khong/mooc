type Stack = String
type Errors = [Int]

main :: IO ()
main = do
    str <- getLine
    putStrLn . parseResult . checkBraces $ str

parseResult :: (Maybe Stack, Errors) -> String
parseResult (Nothing, errors) = show (head errors)
parseResult ((Just stack), errors)
  | null stack = "Success"
  | otherwise = show (head errors)

checkBraces :: String -> (Maybe Stack, Errors)
checkBraces = foldl checkChar (Just [], []) . zip [1..]

checkChar :: (Maybe Stack, Errors) -> (Int, Char) -> (Maybe Stack, Errors)
checkChar (Nothing, errors) _ = (Nothing, errors)
checkChar (Just stack, errors) (n, x)
    | isOpening x = (Just (x:stack), n:errors)
    | isClosing x && null stack = (Nothing, n:errors)
    | isClosing x && bracesMatch (head stack) x = (Just (tail stack), tail errors)
    | isClosing x = (Nothing, n:errors)
    | otherwise = (Just stack, errors)

bracesMatch :: Char -> Char -> Bool
bracesMatch '[' ']' = True
bracesMatch '(' ')' = True
bracesMatch '{' '}' = True
bracesMatch _ _     = False

isOpening :: Char -> Bool
isOpening x = x `elem` "[({"

isClosing :: Char -> Bool
isClosing x = x `elem` "])}"
