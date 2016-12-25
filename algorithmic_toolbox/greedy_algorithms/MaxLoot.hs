import           Control.Monad
import           Data.List
import           Text.Printf

main :: IO ()
main = do
    input <- getLine
    let numItems = parseNumItems input
        capacity = parseCapacity input

    itemsString <- replicateM numItems getLine
    let items = map parseItem itemsString

    printf "%.4f\n" (maxValue capacity items)

type Capacity = Double
type Value = Double
type Weight = Double
type Item = (Value, Weight)
type Knapsack = (Value, Weight)

parseNumItems :: String -> Int
parseNumItems = read . head . words

parseCapacity :: String -> Capacity
parseCapacity = read . last . words

parseItem :: String -> Item
parseItem itemStr =
    let [value, weight] = map read . words $ itemStr
     in (value, weight)

maxValue :: Capacity -> [Item] -> Value
maxValue capacity items =
    let sortedItems = sortBy (flip compareValuePerWeight) items
     in fst $ foldl (greedyLoot capacity) (0, 0) sortedItems

compareValuePerWeight :: Item -> Item -> Ordering
compareValuePerWeight (v0, w0) (v1, w1) = compare (v0 / w0) (v1 / w1)

greedyLoot :: Capacity -> Knapsack -> Item -> Knapsack
greedyLoot capacity (currentValue, currentWeight) (v, w) =
    if (capacity - currentWeight) > w
       then (currentValue + v, currentWeight + w)
       else let frac = (capacity - currentWeight) / w
             in (currentValue + frac * v, currentWeight + frac * w)
