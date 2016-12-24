import           Data.List

main :: IO ()
main = do
    let xs = [9, 3, 1, 38, 38, 8, 3, 4, 1, 0, 446] :: [Int]
    print $ "Expected: " ++ show (sort xs)
    print $ "Merge sort: " ++ show (mergeSort xs)
    print $ "Selection sort: " ++ show (selectionSort xs)
    print $ "Quick sort: " ++ show (quickSort xs)
    print $ "Bubble sort: " ++ show (bubbleSort xs)
    print $ "Insertion sort: " ++ show (insertionSort xs)

mergeSort :: Ord a => [a] -> [a]
mergeSort [x] = [x]
mergeSort xs =
    let splitIx = div (length xs) 2
        (l0, l1) = (take splitIx xs, drop splitIx xs)
     in merge (mergeSort l0) (mergeSort l1)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) =
    if x < y
       then x : merge xs (y:ys)
       else y : merge (x:xs) ys

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs =
    let minX = minimum xs
        rest = delete minX xs
     in minX : selectionSort rest

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let lessThanX = filter (<= x) xs
        moreThanX = filter (> x) xs
     in quickSort lessThanX ++ [x] ++ quickSort moreThanX

bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs =
    let bSwapped = reverse $ foldl reversedBubbleSwap [] xs
     in bubbleSort (init bSwapped) ++ [last bSwapped]

reversedBubbleSwap :: Ord a => [a] -> a -> [a]
reversedBubbleSwap [] y     = [y]
reversedBubbleSwap (x:xs) y = if x <= y then y:x:xs else x:y:xs

insertionSort :: Ord a => [a] -> [a]
insertionSort xs =
    let inserted = foldl insertElement [] xs
     in if inserted == xs then xs else insertionSort inserted

insertElement :: Ord a => [a] -> a -> [a]
insertElement [] y = [y]
insertElement xs y =
    let moreThanIx = [i | (i, x) <- zip [0..] xs, x > y]
        nPivot = if null moreThanIx then length xs else head moreThanIx
     in take nPivot xs ++ [y] ++ drop nPivot xs
