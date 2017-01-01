import           Control.Applicative
import           Control.Monad
import           Data.Monoid

type BSize = Int
type ArrivalT = Int
type ProcessT = Int
type Packet = (ArrivalT, ProcessT)
type Queue = (Int, [ProcessT])

newtype Logged a = Logged { runWriter :: (a, [Int]) } deriving (Show)

instance Monad Logged where
    return a             = Logged (a,mempty)
    (Logged (a,w)) >>= f = let (a',w') = runWriter $ f a in Logged (a',w `mappend` w')

instance Functor Logged where
    fmap f (Logged (x, logs)) = Logged (f x, logs)

instance Applicative Logged where
    pure x = Logged (x, [])
    Logged (f, fLogs) <*> Logged (x, xLogs) = Logged (f x, fLogs ++ xLogs)

dropped :: Int
dropped = (-1)

{---------------------------------------------------------------------}

main :: IO ()
main = do
    (bSize, nPacks) <- fmap parsePair getLine
    packets <- replicateM nPacks (fmap parsePair getLine)
    let (Logged (_, logs)) = solve bSize packets
    mapM_ print logs

parsePair :: String -> (Int, Int)
parsePair str = (x, y) where [x, y] = map read . words $ str

solve :: BSize -> [Packet] -> Logged Queue
solve bSize packets = foldl process emptyQueue packets
    where emptyQueue = return (0, [])
          process queue packet = (queue >>= processPacket bSize packet)

processPacket :: BSize -> Packet -> Queue -> Logged Queue
processPacket bSize (aTime, pTime) queue
  | bSize <= length qs' = Logged (queue', [dropped])
  | otherwise = Logged ((start', qs' ++ [pTime]), [beginTime])
  where queue'@(start', qs') = fastForward queue aTime
        beginTime = start' + sum qs'

fastForward :: Queue -> ArrivalT -> Queue
fastForward (start, qs) aTime =
    let finTimes = tail $ scanl (+) start qs
        nFins = length $ filter (<=aTime) finTimes
        (fqs, qs') = splitAt nFins qs
        start' = if null qs' then aTime else start + sum fqs
    in (start', qs')

{-
bSize = 4
queue@(start, qs) = (10, [5, 2, 10, 10])
packet@(aTime, process) = (15, 1)

queue'@(start', qs') = fastForward queue aTime
fTime = sum (map snd aQueue)

processPacket bSize (start, qs) packet

-}
