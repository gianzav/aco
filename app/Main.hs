module Main where
import qualified Aco
import System.Random
import Data.Graph
import Control.Monad (zipWithM, zipWithM_)


main :: IO ()
main = do
        let steps = 100
        let volatility = 0.5
        let depositedPheromone = 1
        let initPheromone = 1
        let numAnts = 20

        {-
         -      A - B - C
         -      |   |
         -      E - D - F
         -
         -  Source = A
         -  Destination = F
        -}
        let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [
                ("A", 'A', ['B', 'E']),
                ("B", 'B', ['A', 'C', 'D']),
                ("C", 'C', ['B']),
                ("D", 'D', ['B', 'E', 'F']),
                ("E", 'E', ['D', 'A']),
                ("F", 'F', ['D'])
                ]
        randomGenerators <-  mapM (\_ -> initStdGen) [1..numAnts]
        let antList = zipWithM (\_ randGen -> makeAnt randGen vertexFromKey 'A' 'F') [1..numAnts] randomGenerators
        let edgeLabels = Aco.initEdgeLabels graph initPheromone
        case antList of
            (Just ants) ->
                let (_, labels) = Aco.simulate steps graph ants edgeLabels volatility depositedPheromone
                in mapM_ putStrLn (Aco.showEdges labels graph nodeFromVertex)
            Nothing -> putStrLn "Something went wrong in antList initialization"


type VertexKey = Char
makeAnt :: StdGen -> (VertexKey -> Maybe Vertex) -> VertexKey -> VertexKey -> Maybe Aco.Ant
makeAnt randGen vertexFromKey source dest  =
        do source' <- vertexFromKey source
           dest' <- vertexFromKey dest
           return (Aco.Ant source' Aco.Forward [] source' dest' randGen)
