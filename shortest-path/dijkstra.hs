import qualified Data.Map as Map
import Data.List
import System.Environment
import System.IO

data Vertex = Vertex { x :: Int
                     , y :: Int }
                     deriving (Show, Eq, Ord)

data Edge = Edge { a :: Vertex
                 , b :: Vertex
                 , distance :: Int }
                 deriving (Show, Eq, Ord)

type Graph = [Edge]

type DistanceMap = Map.Map Vertex Int

type PreviousMap = Map.Map Vertex Vertex

createVertices :: String -> [Vertex]
createVertices input = foldr
    (\i acc -> Vertex i 0 : Vertex i 1 : acc)
    []
    [0 .. (length $ lines input) `div` 3]

createGraph :: Int -> [String] -> Graph
createGraph n [] = []
createGraph n inputEdges
    | edgeType == 0 = Edge (Vertex i0 0) (Vertex i1 0) distance : Edge (Vertex i1 0) (Vertex i0 0) distance : remaining
    | edgeType == 1 = Edge (Vertex i0 1) (Vertex i1 1) distance : Edge (Vertex i1 1) (Vertex i0 1) distance : remaining
    | edgeType == 2 = Edge (Vertex i1 0) (Vertex i1 1) distance : Edge (Vertex i1 1) (Vertex i1 0) distance : remaining
    where edgeType  = n `mod` 3
          i0        = n `div` 3
          i1        = succ i0
          distance  = read $ head inputEdges
          remaining = createGraph (succ n) (tail inputEdges)

neighborVertices :: Vertex -> [Vertex]
neighborVertices (Vertex x y) = [Vertex x (succ y)
                                ,Vertex x (pred y)
                                ,Vertex (succ x) y
                                ,Vertex (pred x) y]

getNeighbors :: Vertex -> Graph -> [Vertex]
getNeighbors source graph = filter (\v -> isNeighbor v source graph) $ neighborVertices source

isNeighbor :: Vertex -> Vertex -> Graph -> Bool
isNeighbor candidate source [] = False
isNeighbor candidate source ((Edge a b _):vs) =
    (a == candidate && b == source) || isNeighbor candidate source vs

sortByDistance :: DistanceMap -> Vertex -> Vertex -> Ordering
sortByDistance dist a b
    | distA == distB = EQ
    | distA <  distB = LT
    | otherwise      = GT
    where distA = dist Map.! a
          distB = dist Map.! b

getDist :: Graph -> Vertex -> Vertex -> Int
getDist graph source dest =
    distance $ head $ filter (\edge -> (a $ edge) == source && (b $ edge) == dest) graph

updateNeighbor :: Graph -> Vertex -> Vertex -> DistanceMap -> PreviousMap -> (DistanceMap, PreviousMap)
updateNeighbor graph source neighbor dist prev =
    let alt = dist Map.! source + getDist graph source neighbor
    in  if alt < dist Map.! neighbor then (Map.insert neighbor alt dist, Map.insert neighbor source prev) else (dist, prev)

updateNeighbors :: Graph -> Vertex -> [Vertex] -> DistanceMap -> PreviousMap -> (DistanceMap, PreviousMap)
updateNeighbors graph source [] dist prev = (dist, prev)
updateNeighbors graph source (x:xs) dist prev =
    let (nextDist, nextPrev) = updateNeighbor graph source x dist prev
    in updateNeighbors graph source xs nextDist nextPrev

shortestPath :: Graph -> [Vertex] -> DistanceMap -> PreviousMap -> (DistanceMap, PreviousMap)
shortestPath graph [] dist prev = (dist, prev)
shortestPath graph unvisited dist prev =
    let (u:nextUnvisited)    = sortBy (sortByDistance dist) unvisited
        neighbors            = getNeighbors u graph
        (nextDist, nextPrev) = updateNeighbors graph u neighbors dist prev
    in  shortestPath graph nextUnvisited nextDist nextPrev

buildPath :: PreviousMap -> Vertex -> [Vertex]
buildPath prev goal = case (Map.lookup goal prev) of
    Just source -> (buildPath prev source) ++ [goal]
    Nothing     -> [goal]

sortByPath :: DistanceMap -> DistanceMap -> Vertex -> Vertex -> Ordering
sortByPath destCosts destPaths a b =
    let costOrder = sortByDistance destCosts a b
        pathOrder = sortByDistance destPaths a b
    in  if costOrder == EQ then pathOrder else costOrder

pickOptimal :: DistanceMap -> PreviousMap -> Vertex
pickOptimal dist prev =
    let vertices     = Map.keys dist
        lastStop     = maximum $ map x vertices
        destinations = filter (\v -> (x $ v) == lastStop) vertices
        destCosts    = Map.fromList $ map (\v -> (v, dist Map.! v)) destinations
        destPaths    = Map.fromList $ map (\v -> (v, length $ buildPath prev v)) destinations
    in  head $ sortBy (sortByPath destCosts destPaths) destinations

main = do
    args <- getArgs
    input <- readFile $ head args
    let edges        = lines input
        graph        = createGraph 0 edges
        vertices     = createVertices input
        distances    = Map.fromList $ map (\v -> (v, if (x $ v) == 0 then 0 else maxBound :: Int)) vertices 
        previous     = Map.empty :: PreviousMap
        (dist, prev) = shortestPath graph vertices distances previous
        best         = pickOptimal dist prev
    putStrLn $ (show $ dist Map.! best) ++ " " ++ (show $ buildPath prev best)

