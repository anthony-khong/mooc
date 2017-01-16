import           Control.Monad
import           Control.Monad.ST
import           Data.Array       ((!), (//))
import qualified Data.Array       as A
import qualified Data.Array.ST    as S
import           Data.STRef

type Arr = A.Array Int Int
type STArr s = S.STArray s Int Int
data DSet = DSet { parents :: Arr
                 , ranks   :: Arr
                 } deriving (Show)

main :: IO ()
main = do
    print ()

cFind :: Int -> DSet -> (DSet, Int)
cFind i (DSet parents ranks) = (DSet parents' ranks, parents' ! i)
    where parents' = compressPath i parents

compressPath :: Int -> Arr -> Arr
compressPath i xs = S.runSTArray $ do
    let root = find i xs
    arr <- newSTArray xs
    compress root i arr

compress :: Int -> Int -> STArr s -> ST s (STArr s)
compress root i arr = do
    parent <- S.readArray arr i
    if parent /= i
       then do
           S.writeArray arr i root
           compress root parent arr
        else return arr

find :: Int -> Arr -> Int
find i xs = if parent /= i
               then find parent xs
               else i
    where parent = xs ! i

union :: Int -> Int -> DSet -> DSet
union i j dset = DSet newParents newRanks
    where (dset', parI) = cFind i dset
          (dset'', parJ) = cFind j dset'
          (DSet ps rs) = dset''
          (ri, rj) = (rs ! i, rs ! j)
          newParents = if parI == parJ
                          then ps
                          else if ri > rj
                               then ps // [(parJ, parI)]
                               else ps // [(parI, parJ)]
          newRanks = if (parI /= parJ) && (ri == rj)
                        then rs // [(j, rj + 1)]
                        else rs

newSTArray :: Arr -> ST s (STArr s)
newSTArray xs = S.newListArray (0, n-1) xList
    where xList = A.elems xs
          n = length xList
