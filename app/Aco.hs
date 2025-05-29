module Aco where

import System.Random
import Data.Graph (Graph, Vertex, Edge, edges)
import qualified Data.Map.Strict as M
import Data.Array
import Control.Monad.Random

-- Generic label
newtype Label a = Label a
    deriving (Eq, Show)

data Direction = Forward | Backward
    deriving (Eq,Show)

type Source = Vertex
type Destination = Vertex
type Path = [Vertex]
data Ant = Ant Vertex Direction Path Source Destination
    deriving (Eq, Show)

type Pheromone = Float              -- pheromone left by Ants
type Volatility = Float             -- pheromone volatility

type AntVertexLabel = Label [Ant]   -- label for vertices, a collection of ants
type AntEdgeLabel = Label Pheromone -- label for edges, with the total deposited pheromone


type EdgeLabels = M.Map Edge AntEdgeLabel

getEdgeLabel :: Edge -> EdgeLabels -> AntEdgeLabel
getEdgeLabel e m = m M.! e -- calls error if the edge is not present

setEdgeLabel :: Edge -> AntEdgeLabel -> EdgeLabels -> EdgeLabels
setEdgeLabel = M.insert


neighbors :: Graph -> Vertex -> [Vertex]
neighbors g v = g ! v -- a Graph is an Array Vertex [Vertex]

outEdges :: Graph -> Vertex -> [Edge]
outEdges g v = map (\u -> (v,u))  (neighbors g v)

{-
 - All the Ants take atomically a step towards their direction.
 - When an Ant going Forward reaches its Destination, it changes its direction to Backward, and removes the loops from its memorised Path.
 - When an Ant is moving Backward, it leaves a Pheromone on the Edge it traversed.
 - The choice of the Edge to follow during the Ant movement is influenced by the Pheromone on the neighbors of the current Vertex.
 - When an Ant going Backward reaches the Source, its Direction becomes Backward.
-}
-- takeStep :: Graph -> VertexLabels -> VertexLabels

weightedRandomChoice :: RandomGen g => g -> [(a, Rational)] -> (g,a)
weightedRandomChoice randGen weights = (g,a)
    where m = fromList weights
        -- this is not needed but otherwise I need to change all the functions using weightedRandomChoice
          (a,g) = runRand m randGen 

-- Gives the weighted list of possible next vertices for an Ant, given the
-- Pheromone on the Edge(s)
nextVertices :: [Edge] -> EdgeLabels -> [(Vertex, Pheromone)]
nextVertices es labels = map getWeightedVertex es
    where
    getWeightedVertex :: Edge -> (Vertex, Pheromone)
    getWeightedVertex e@(_,v) = let (Label p) = getEdgeLabel e labels
                                in (v, p)

removeLoops :: Path -> Path
removeLoops = id

nextVertex :: RandomGen g  => g -> [Edge] -> EdgeLabels -> (g, Vertex)
nextVertex randGen es edgeLabels =
    -- convert pheromone level to Rational
    let possibleNext = map (\(x,p) -> (x, toRational p)) (nextVertices es edgeLabels)
    in weightedRandomChoice randGen possibleNext

-- Positive or negative Pheromone change on an Edge
data PheromoneUpdate = PU Edge Pheromone


antStep :: RandomGen g => g -> Graph -> EdgeLabels -> Ant -> Pheromone -> (g, (Ant, PheromoneUpdate))
antStep randGen g edgeLabels a@(Ant v Forward path s d) pheromone =
    -- don't consider the last visited Vertex
    let butLastVisited = case path of
                            [] -> outEdges g v
                            _ -> case filter (\(_,y) -> y /= last path) (outEdges g v) of
                                [] -> [(v, last path)] -- take the last edge only if there is no other option
                                x -> x
        possibleEdges = butLastVisited
        (newRandGen, nxt) = nextVertex randGen possibleEdges edgeLabels
        in if nxt == d then
           (newRandGen, (Ant nxt Backward (removeLoops (path ++ [v])) s d, PU (v,nxt) 0))
          else
           (newRandGen, (Ant nxt Forward (path ++ [v]) s d, PU (v,nxt) 0))
antStep randGen _ _ a@(Ant v Backward path s d) pheromone =
    -- don't consider the last visited Vertex
    let nxt = case path of
                [] -> error ("No vertices in path of ant " ++ show a)
                _ -> last path
    in if nxt == s then
        (randGen, (Ant nxt Forward [] s d, PU (v,nxt) pheromone))
       else
        (randGen, (Ant nxt Backward (init path) s d, PU (v,nxt) pheromone))

graphStep :: RandomGen g => g -> Graph -> [Ant] -> EdgeLabels -> Pheromone -> (g, ([Ant], EdgeLabels))
graphStep randGen g [] edgeLabels _ = (randGen, ([], edgeLabels))
graphStep randGen g (ant:ants) edgeLabels pheromone = (newRandGen, (finalAnts, updatedEdgeLabels))
    where (tempRandGen, (newAnts, newEdgeLabels)) = graphStep randGen g ants edgeLabels pheromone
          (newRandGen, (newAnt, update)) = antStep tempRandGen g edgeLabels ant pheromone
          finalAnts = newAnt:newAnts
          updatedEdgeLabels = updateEdgeLabel update newEdgeLabels

evaporate :: EdgeLabels -> Volatility -> EdgeLabels
evaporate edgeLabels v = M.map (\(Label p) -> Label (p - (p * v))) edgeLabels



updateEdgeLabel :: PheromoneUpdate -> EdgeLabels -> EdgeLabels
updateEdgeLabel (PU (u,v) pu) els =
    -- need to update the edge in both directions
    let (Label old) = getEdgeLabel (u,v) els
        (Label old') = getEdgeLabel (v,u) els
        els' = setEdgeLabel (u,v) (Label $ old + pu) els
        in setEdgeLabel (v,u) (Label $ old' + pu) els'


initEdgeLabels :: Graph -> Pheromone -> Aco.EdgeLabels
initEdgeLabels g p = foldr initPheromone M.empty (edges g)
    where
        initPheromone edge labels = Aco.setEdgeLabel edge (Label p) labels


simulate :: RandomGen g => g -> Int -> Graph -> [Ant] -> EdgeLabels -> Volatility -> Pheromone -> ([Ant], EdgeLabels)
simulate _ 0 _ ants edgeLabels _ _ = (ants, edgeLabels)
simulate randGen steps g ants edgeLabels volatility pheromone =
    let (newRandGen, (newAnts, newEdgeLabels)) = graphStep randGen g ants (evaporate edgeLabels volatility) pheromone
    in simulate newRandGen (steps-1) g newAnts newEdgeLabels volatility pheromone

showEdges :: Show key => EdgeLabels -> Graph -> (Vertex -> (node, key, [key])) -> [String]
showEdges edgeLabels g f = map printEdge (edges g)
    where printEdge (u,v) =
            let (_, u', _) = f u
                (_, v', _) = f v
                pheromone = Aco.getEdgeLabel (u,v) edgeLabels
            in "(" ++ show u' ++ "," ++ show v' ++ ")" ++ ":" ++ show pheromone
