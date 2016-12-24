module ArrayLib where

import qualified Data.Array as A

type Arr a = A.Array Int a

toArray :: [a] -> Arr a
toArray xs = A.listArray (0, length xs - 1) xs

length' :: Arr a -> Int
length' = (+1) . negate . uncurry (-) . A.bounds

null' :: Arr a -> Bool
null' xs = length' xs == 0

take' :: Int -> Arr a -> Arr a
take' n xs = A.ixmap (0, m - 1) id xs
    where m = min n (length' xs)

drop' :: Int -> Arr a -> Arr a
drop' n xs = A.ixmap (0, m - 1) (+n) xs
    where m = max 0 (length' xs - n)

head' :: Arr a -> a
head' xs = xs A.! 0

tail' :: Arr a -> Arr a
tail' = drop' 1

last' :: Arr a -> a
last' xs = xs A.! (length' xs - 1)

init' :: Arr a -> Arr a
init' xs = take' (length' xs - 1) xs

splitAt' :: Int -> Arr a -> (Arr a, Arr a)
splitAt' n xs = (take' n xs, drop' n xs)
