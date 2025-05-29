module Main where
import qualified Aco
import System.Random
import Data.Graph


main :: IO ()
main = do
        randGen <- initStdGen
        let steps = 100
        let volatility = 0.5
        let pheromone = 1
        let initPheromone = 1

        {-
         -      A - B - C
         -          |
         -      E - D - F
         -
         -  Source = A
         -  Destination = F
        -}
        let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [
                ("A", 'A', ['B']),
                ("B", 'B', ['A', 'C', 'D']),
                ("C", 'C', ['B']),
                ("D", 'D', ['B', 'E', 'F']),
                ("E", 'E', ['D']),
                ("F", 'F', ['D'])
                ]
        let antList = mapM (\_ -> makeAnt vertexFromKey 'A' 'F') [1..20]
        let edgeLabels = Aco.initEdgeLabels graph initPheromone
        case antList of
            (Just ants) ->
                let (_, labels) = Aco.simulate randGen steps graph ants edgeLabels volatility pheromone
                in mapM_ putStrLn (Aco.showEdges labels graph nodeFromVertex)
            Nothing -> putStrLn "Something went wrong in antList initialization"


type VertexKey = Char
makeAnt :: (VertexKey -> Maybe Vertex) -> VertexKey -> VertexKey -> Maybe Aco.Ant
makeAnt vertexFromKey source dest  =
        do source' <- vertexFromKey source
           dest' <- vertexFromKey dest
           return (Aco.Ant source' Aco.Forward [] source' dest')
