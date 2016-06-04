import Duck
import Data.List

slowSort :: [Int] -> [Int]
slowSort [] = []
slowSort xs = mn : (slowSort $ delete mn xs)
  where mn = (minimum xs)

fastSort :: [Int] -> [Int]
fastSort = (map reverse) . sort

headerText :: String
headerText = "\t-- Duck - Complexity Tester --\n\n" ++
             " Testing sorting algorithms\n" ++
             "   fastSort: haskell default sort\n" ++
             "   slowSort: selection sort\n"

main = do  
  let t = defaultParam {range = (100, 1000),
                        iterations = 15}

  putStrLn headerText

  rfast <- runSingleTest t (fastSort)
  rslow <- runSingleTest t (slowSort)

  putStrLn ""
  putStrLn $ "Reporting fast with " ++
    (show $ pretty $ outlierEffect rfast) ++
    "% of outside effects"
  putStrLn $ "Reporting slow with " ++
    (show $ pretty $ outlierEffect rslow) ++
    "% of outside effects"

  let hypList = [
        hypothesis "linear" hp,
        hypothesis "linearitmic" hpln,
        hypothesis "quadratic" hp2,
        hypothesis "cubic" hp3]

  putStrLn $ ("fastSort: " ++) $ pbest $ map fst $ testGroup rfast hypList
  putStrLn $ ("slowSort: " ++) $ pbest $ map fst $ testGroup rslow hypList

  where pretty x = round $ x * 100
        pbest = intercalate ", "
