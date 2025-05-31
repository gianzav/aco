module Aco where

import System.Random
import Data.Graph (Graph, Vertex, Edge, edges)
import qualified Data.Map.Strict as M
import Data.Array
import Control.Monad.Random
import Data.Tuple (swap)
import GHC.Stack (HasCallStack)
import qualified Data.Set as Set
import Debug.Trace (trace)

data Direction = Forward | Backward
    deriving (Eq,Show)

type Source = Vertex
type Destination = Vertex
type Path = [Vertex]

data Ant = Ant
    {
        antPosition :: Vertex,
        antDirection :: Direction,
        antPath :: Path,
        antSrc :: Source,
        antDest :: Destination,
        antRnd :: StdGen
    }
    deriving (Eq, Show)

type Pheromone = Float              -- pheromone left by Ants
type Volatility = Float             -- pheromone volatility rate

type AntVertexLabel = [Ant]         -- label for vertices, a collection of ants
type AntEdgeLabel = Pheromone       -- label for edges, with the total deposited pheromone

data LabeledGraph a b = LG
    {
        lgGraph :: Graph,
        lgVertexLabels :: M.Map Vertex a,
        lgEdgeLabels :: M.Map Edge b
    }
    deriving (Show, Eq)

type AntGraph = LabeledGraph AntVertexLabel AntEdgeLabel
type EdgeLabels = M.Map Edge AntEdgeLabel

getEdgeLabel :: HasCallStack => Edge -> EdgeLabels -> AntEdgeLabel
getEdgeLabel e m = case M.lookup e m of
                    (Just label) -> label
                    Nothing -> error ("Edge label of " ++ show e ++ " not found.")

setEdgeLabel :: Edge -> AntEdgeLabel -> EdgeLabels -> EdgeLabels
setEdgeLabel = M.insert

neighbors :: Graph -> Vertex -> [Vertex]
neighbors g v = g ! v -- a Graph is an Array Vertex [Vertex]

-- Outgoing edges of a vertex.
outEdges :: Graph -> Vertex -> [Edge]
outEdges g v = [(v,u) | u <- neighbors g v]

-- Extract randomly an element from a list, where each possible choice has a weight.
-- The higher the weight, the higher the probability of an element to be extracted.
weightedRandomChoice :: RandomGen g => g -> [(a, Rational)] -> (g,a)
weightedRandomChoice randGen weights = swap $ runRand m randGen
  where m = fromList weights

-- Gives the weighted list of possible next vertices for an Ant, given the
-- Pheromone on the Edge(s).
possibleNextVertices :: [Edge] -> EdgeLabels -> [(Vertex, Pheromone)]
possibleNextVertices es labels = map getWeightedVertex es
  where
    getWeightedVertex :: Edge -> (Vertex, Pheromone)
    getWeightedVertex e@(_,v) = let p = getEdgeLabel e labels
                                in (v, p)

-- Choose randomly a next vertex based on the pheromone on the edges
nextVertex :: RandomGen g  => g -> [Edge] -> EdgeLabels -> (g, Vertex)
nextVertex randGen es edgeLabels =
    -- convert pheromone level to Rational
    let possibleNext = [(x, toRational p) | (x,p) <- possibleNextVertices es edgeLabels]
    in weightedRandomChoice randGen possibleNext

-- Remove all loops from a path
removeLoops :: (Ord a, Eq a) => [a] -> [a]
removeLoops = go Set.empty []
  where
    go _ loopFree [] = loopFree
    go seen loopFree (x:xs)
      | x `Set.member` seen =
          let newloopFree = dropWhile (/= x) (reverse loopFree)
              newSeen = Set.fromList newloopFree
          in go newSeen (reverse newloopFree) xs
      | otherwise = go (Set.insert x seen) (loopFree ++ [x]) xs


-- Positive or negative Pheromone change on an Edge
data PheromoneUpdate = PU Edge Pheromone
    deriving (Show, Eq)

{-
 - All the Ants take atomically a step towards their direction.
 - When an Ant going Forward reaches its Destination, it changes its direction to Backward, and removes the loops from its memorised Path.
 - When an Ant is moving Backward, it leaves a Pheromone on the Edge it traversed.
 - The choice of the Edge to follow during the Ant movement is influenced by the Pheromone on the neighbors of the current Vertex.
 - When an Ant going Backward reaches the Source, its Direction becomes Backward.
-}
antStep :: AntGraph -> Ant -> Pheromone -> (Ant, PheromoneUpdate)
antStep (LG g _ edgeLabels) (Ant v Forward path src dst randGen) _ =
    -- don't consider the last visited Vertex
    let possibleEdges = case path of
                        -- no step has been taken, all neighbors are feasible
                        [] -> outEdges g v
                        _ -> case filter (\(_,y) -> y /= last path) (outEdges g v) of
                            -- take the last traversed edge only if there is no other option
                            [] -> [(v, last path)]
                            -- otherwise use the outgoing edges, minus the last traversed one
                            x -> x
        (newRandGen, nxt) = nextVertex randGen possibleEdges edgeLabels
    in if nxt == dst then
        -- When the destination vertex is reached, remove the loops from the traversed path
        -- and prepare to move backwards
        (Ant nxt Backward (removeLoops (path ++ [v])) src dst newRandGen, PU (v,nxt) 0)
       else
        (Ant nxt Forward (path ++ [v]) src dst newRandGen, PU (v,nxt) 0)
antStep _ a@(Ant v Backward path src dst randGen) pheromone =
    -- Moving backwards, the next step is the last in the forward path
    let nxt = case path of
                [] -> error ("No vertices in path of ant " ++ show a)
                _ -> last path
    in if nxt == src then
        -- When the source vertex is reached, start exploring again
        -- Moving backwards, leave the pheromone on the edge
        (Ant nxt Forward [] src dst randGen, PU (v,nxt) pheromone)
       else
        -- Otherwise, keep going back towards the source and leave pheromone
        (Ant nxt Backward (init path) src dst randGen, PU (v,nxt) pheromone)

-- Let all the ants on the graph take a step
graphStep :: AntGraph -> [Ant] -> Pheromone -> ([Ant], AntGraph)
graphStep g ants phero = graphStep' g ants phero []

graphStep' :: AntGraph -> [Ant] -> Pheromone -> [Ant] -> ([Ant], AntGraph)
graphStep' g [] _ finalAnts = (finalAnts, g)
graphStep' lg@(LG g vL eL) (a:as) pheromone finalAnts =
    let (newAnt, update) = antStep lg a pheromone
        updatedGraph = LG g vL (updateEdgeLabel update eL)
    in graphStep' updatedGraph as pheromone (newAnt:finalAnts)

-- Apply the PheromoneUpdate on an edge
updateEdgeLabel :: PheromoneUpdate -> EdgeLabels -> EdgeLabels
updateEdgeLabel (PU (u,v) pu) edgeLabels =
    -- need to update the edge in both directions
    let old = getEdgeLabel (u,v) edgeLabels
        old' = getEdgeLabel (v,u) edgeLabels
        els' = setEdgeLabel (u,v) (old + pu) edgeLabels
        in setEdgeLabel (v,u) (old' + pu) els'

-- Initialize the pheromone on the edges with a fixed value
initEdgeLabels :: AntGraph -> Pheromone -> AntGraph
initEdgeLabels (LG g vL eL) phero = LG g vL (foldr initPheromone eL (edges g))
  where
    initPheromone edge = setEdgeLabel edge phero

-- Evaporate pheromone from all the edges
evaporate :: EdgeLabels -> Volatility -> EdgeLabels
evaporate edgeLabels v = M.map (\phero -> phero - (phero * v)) edgeLabels

-- Perform the algorithm for a given number of steps
simulate :: Int -> AntGraph -> [Ant] -> Volatility -> Pheromone -> ([Ant], AntGraph)
simulate 0 g ants _ _ = (ants, g)
simulate steps lg ants volatility pheromone =
    -- let all ants take a step
    let (newAnts, LG g' vL eL) = graphStep lg ants pheromone
    -- then evaporate the pheromone
        newGraph = LG g' vL (evaporate eL volatility)
    in simulate (steps-1) newGraph newAnts volatility pheromone

-- Pretty print the edges showing the label
prettyEdges :: Show key => AntGraph -> (Vertex -> (node, key, [key])) -> [String]
prettyEdges (LG g _ edgeLabels) f = map printEdge (edges g)
  where
    printEdge (u,v) =
        let (_, u', _) = f u
            (_, v', _) = f v
            pheromone = Aco.getEdgeLabel (u,v) edgeLabels
        in "(" ++ show u ++ ":" ++ show u' ++ "->" ++ show v ++ ":" ++ show v' ++ ")" ++ ":" ++ show pheromone
