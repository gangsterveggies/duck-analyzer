{-
Matrix example.

Check README.md for more info
-}

import Duck

import Data.List
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

newtype MatrixMul = MatrixMul (Matrix Int, Matrix Int)

instance Sized MatrixMul where
  dimSize (MatrixMul (a, b)) = max (dimSize a) (dimSize b)

instance Cased MatrixMul where
  genCase sz = do
    a <- genCase szeach
    b <- genCase szeach
    let b' = Matrix.transpose b
    return (MatrixMul (a, b'))
      where szeach = sz `div` 2

multiplyNaive :: MatrixMul -> Matrix Int
multiplyNaive (MatrixMul (a, b)) = Matrix.multStd a b

headerText :: String
headerText = "\t-- Duck - Complexity Tester --\n\n" ++
             " Plotting matrices algorithms\n" ++
             "   multiplyNaive: naive cubic matrix multiplication\n"

main = do  
  let t = defaultParam {range = (50, 400),
                        iterations = 15}

  putStrLn headerText

  rnaive <- runSingleTest t (multiplyNaive)

  putStrLn ""
  putStrLn $ "Reporting naive with " ++
    (show $ pretty $ outlierEffect rnaive) ++
    "% of outside effects (" ++
    (outlierReport $ outlierEffect rnaive) ++
    ")"

  let hypList = [
        hypothesis "linear" hp,
        hypothesis "linearitmic" hpln,
        hypothesis "quadratic" hp2,
        hypothesis "cubic" hp3]

  putStrLn $ ("naive: " ++) $ pbest $ map fst $ testGroup rnaive hypList (topConf 2)

  plotHypothesis rnaive hp2 (testHypothesis hp2 rnaive) "quadratic"
  plotHypothesis rnaive hp3 (testHypothesis hp3 rnaive) "cubic"
  where pretty x = round $ x * 100
        pbest = intercalate ", "
