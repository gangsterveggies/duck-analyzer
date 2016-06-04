{-
Graph example

Check README.md for more info
-}

import Duck

import Data.List
import Data.Graph (Graph, Vertex, Forest)
import qualified Data.Graph as Graph
import qualified Test.QuickCheck as QC

dfs :: Graph -> Forest Vertex
dfs = Graph.dff

scc :: Graph -> Forest Vertex
scc = Graph.scc

headerText :: String
headerText = "\t-- Duck - Complexity Tester --\n\n" ++
             " Testing graph algorithms\n" ++
             "   dfs:   depth first search from each vertex\n" ++
             "   scc:   strongly connected components\n"

main = do  
  let t = defaultParam {range = (100, 600),
                        iterations = 20,
                        timePerTest = 3.0}

  putStrLn headerText

  rdfs   <- runSingleTest t (dfs)
  rscc   <- runSingleTest t (scc)

  putStrLn ""
  putStrLn $ "Reporting dfs with " ++
    (show $ pretty $ outlierEffect rdfs) ++
    "% of outside effects (" ++
    (outlierReport $ outlierEffect rdfs) ++
    ")"
  putStrLn $ "Reporting scc with " ++
    (show $ pretty $ outlierEffect rscc) ++
    "% of outside effects (" ++
    (outlierReport $ outlierEffect rscc) ++
    ")"

  let hypList = [
        hypothesis "linear" hp,
        hypothesis "linearitmic" hpln,
        hypothesis "quadratic" hp2,
        hypothesis "cubic" hp3]

  putStrLn $ ("dfs: " ++) $ pbest $ map fst $ testGroup rdfs hypList (maxConf)
  putStrLn $ ("scc: " ++) $ pbest $ map fst $ testGroup rscc hypList (maxConf)

  putStrLn "dfs individual results"
  writeComp rdfs
  putStrLn "scc individual results"
  writeComp rscc

  where pretty x = round $ x * 100
        pbest = intercalate ", "
        writeComp r = mapM_ putStrLn [("linear " ++) $ show $ pretty $
                                      confidence $ testHypothesis hp r,
                                      ("quadratic " ++) $ show $ pretty $ 
                                      confidence $ testHypothesis hp2 r]
