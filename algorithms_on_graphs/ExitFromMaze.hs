import           Control.Monad
import           Control.Monad.State
import qualified Data.Map            as M

main :: IO ()
main = do
    [nVertices, nEdges] <- fmap parseInts getLine
    edges <- fmap (map parseEdge) (replicateM nEdges getLine)
    let vs = toVertices nVertices edges
        exs = explore vs 1

    print (nVertices, nEdges)
    print edges
    print vs
    print exs

parseInts :: String -> [Int]
parseInts = fmap read . words

parseEdge :: String -> Edge
parseEdge = toPair . parseInts
    where toPair xs = (head xs, last xs)

type Key = Int
type Neighbour = Key
type Edge = (Key, Key)
type Vertices = M.Map Key [Neighbour]

toVertices :: Int -> [Edge] -> Vertices
toVertices nVertices = foldl step initMap
    where initMap = M.fromList [(k, []) | k <- [1..nVertices]]
          step vs (u, v) = M.adjust (v:) u . M.adjust (u:) v $ vs

type VisitedState = M.Map Key Bool

initialState :: Int -> VisitedState
initialState nVertices = M.fromList [(k, False) | k <- [1..nVertices]]

explore :: Key -> Vertices -> State VisitedState [Key]
explore key vMap = do
    visitedMap <- gets (M.adjust (const True) key)
    let neighbours = vMap M.! key
        unvisited = filter (not . (visitedMap M.!)) neighbours
        nUnvisited = length unvisited
    if null unvisited
       then return [a]
       else do
           explore


{-
explore :: Vertices -> Key -> [Key]
explore vs = recExplore initExpMap vs
    where initExpMap = M.fromList [(k, False) | k <- [1..nVertices]]
          nVertices = M.size vs

recExplore :: M.Map Key Bool -> Vertices -> Key -> [Key]
recExplore expMap vs key =
    let neighbours = vs M.! key
        unvisited = filter (not . (expMap M.!)) neighbours
        key' = if null unvisited then key else head unvisited
     in if null unvisited
           then []
           else recExplore (M.adjust not key' expMap) vs key'
-}

{-
vs = M.fromList [(1,[4,2]),(2,[3,1]),(3,[4,2]),(4,[1,3])]
nVertices = M.size vs
initExpMap = M.fromList [(k, False) | k <- [1..nVertices]]

4 4
1 2
3 2
4 3
1 4

-}
