import           Control.Monad

type BSize = Int
type ATime = Int
type PTime = Int
type Packet = (ATime, PTime)
type Queue = (Int, [Packet])
type Outcome = Int

dropped :: Int
dropped = (-1)

newtype Logged a = Logged { runWriter :: (a, [Int]) } deriving (Show)

instance Monad Logged where
    return a             = Logged (a,mempty)
    (Logged (a,w)) >>= f = let (a',w') = runWriter $ f a in Logged (a',w `mappend` w')

instance Functor Logged where
    fmap f (Logged (x, logs)) = Logged (f x, logs)

instance Applicative Logged where
    pure x = Logged (x, [])
    Logged (f, fLogs) <*> Logged (x, xLogs) = Logged (f x, fLogs ++ xLogs)

{---------------------------------------------------------------------}

main :: IO ()
main = do
    (bSize, nPacks) <- fmap parsePair getLine
    packets <- replicateM nPacks (fmap parsePair getLine)
    {-print (solve bSize packets)-}
    print ()

parsePair :: String -> (Int, Int)
parsePair str = (x, y) where [x, y] = map read . words $ str

{-solve :: BSize -> [Packet] -> [Outcome]-}
{-solve bSize packets = scanl (processPacket bSize) emptyQueue packets-}
    {-where emptyQueue = []-}

processPacket :: BSize -> Queue -> Packet -> Logged (Int, Queue)
processPacket bSize queue p@(aTime, _)
  | bSize < length aQueue = Logged ((aTime, aQueue), [dropped])
  | otherwise = Logged ((aTime, aQueue ++ [p]), [aTime + fTime])
  where aQueue = queueAtArrival queue aTime
        fTime = sum (map snd aQueue)

{-DEBUG: What's the state of the queue when the packet arrives? This function should just fast-forward the queue. -}
queueAtArrival :: Queue -> ATime -> Queue
queueAtArrival (sTime, qs) aTime =
    let pTimes = map snd qs
        fTimes = scanl (+) sTime $ pTimes
        lenQueue = length . filter (>aTime) $ fTimes
     in drop (length qs - lenQueue) qs

{-
bSize = 4
queue@(sTime, qs) = (10, [(3, 2), (5, 5), (6, 10), (8, 10)])
packet@(aTime, pTime) = (15, 1)
{-processPacket bSize (sTime, qs) packet-}

aQueue = queueAtArrival queue aTime
fTime = sum (map snd aQueue)
-}
