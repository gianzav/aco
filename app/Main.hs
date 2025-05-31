module Main where
import qualified Aco
import System.Random
import Data.Graph
import Data.Map.Strict as M
import Control.Monad (zipWithM, replicateM)


main :: IO ()
main = do
    -- Configuration variables
    let steps = 10000
    let volatility = 0.1
    let depositedPheromone = 1
    let initPheromone = 1
    let numAnts = 20
    let baseGraph = g1

    -- Initialize ant graph
    randomGenerators <- replicateM numAnts initStdGen
    let GP graph nodeFromVertex vertexFromKey srcKey dstKey = baseGraph
    let antList = zipWithM (\_ randGen -> makeAnt randGen vertexFromKey srcKey dstKey) [1..numAnts] randomGenerators
    let labeledGraph = Aco.initEdgeLabels (Aco.LG graph M.empty M.empty) initPheromone

    -- Run the algorithm
    case antList of
        (Just ants) ->
            let (endAnts, endGraph) = Aco.simulate steps labeledGraph ants volatility depositedPheromone
            in do
                mapM_ putStrLn (Aco.prettyEdges endGraph nodeFromVertex)
                mapM_ print endAnts
        Nothing -> putStrLn "Something went wrong in antList initialization"


type VertexKey = Char
makeAnt :: StdGen -> (VertexKey -> Maybe Vertex) -> VertexKey -> VertexKey -> Maybe Aco.Ant
makeAnt randGen vertexFromKey source dest  = do
    source' <- vertexFromKey source
    dest' <- vertexFromKey dest
    return (Aco.Ant source' Aco.Forward [] source' dest' randGen)

data GraphProblem = GP
    {
        gpGraph :: Graph,
        gpNodeFromVertex :: Vertex -> (String, Char, [Char]),
        gpVertexFromKey :: Char -> Maybe Vertex,
        gpSrc :: VertexKey,
        gpDest :: VertexKey
    }

{-
 -      A - B - C
 -      |   |
 -      E - D - F
 -
 -  Source = A
 -  Destination = F
-}
g1 = GP g nfv vfk 'A' 'F'
  where
    (g, nfv, vfk) = graphFromEdges
        [
            ("A", 'A', ['B', 'E']),
            ("B", 'B', ['A', 'C', 'D']),
            ("C", 'C', ['B']),
            ("D", 'D', ['B', 'E', 'F']),
            ("E", 'E', ['D', 'A']),
            ("F", 'F', ['D'])
        ]

{-
 -      A - B
 -
 -  Source = A
 -  Destination = B
-}
g2 = GP g nfv vfk 'A' 'B'
  where
    (g, nfv, vfk) = graphFromEdges
        [
            ("A", 'A', ['B']),
            ("B", 'B', ['A'])
        ]

{-
 -      A - B - C
 -
 -  Source = A
 -  Destination = C
-}
g3 = GP g nfv vfk 'A' 'C'
  where
    (g, nfv, vfk) = graphFromEdges
        [
            ("A", 'A', ['B']),
            ("B", 'B', ['A','C']),
            ("C", 'C', ['B'])
        ]
