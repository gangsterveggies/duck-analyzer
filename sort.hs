{-
Sort example

Check README.md for more info.
-}

import Duck

import Data.List

fastSort :: [Int] -> [Int]
fastSort = sort

slowSort :: [Int] -> [Int]
slowSort [] = []
slowSort xs = mn : (slowSort $ delete mn xs)
  where mn = (minimum xs)

stringSort :: [String] -> [String]
stringSort = sort

headerText :: String
headerText = "\t-- Duck - Complexity Tester --\n\n" ++
             " Testing sorting algorithms\n" ++
             "   fastSort:   haskell default sort\n" ++
             "   slowSort:   selection sort\n" ++
             "   stringSort: default sort on strings\n"

main = do  
  let t = defaultParam {range = (100, 2000),
                        iterations = 15}

  putStrLn headerText

  rfast <- runSingleTest t (fastSort)
  rslow <- runSingleTest t (slowSort)
  rstring <- runSingleTest t (stringSort)

  putStrLn ""
  putStrLn $ "Reporting fast with " ++
    (show $ pretty $ outlierEffect rfast) ++
    "% of outside effects (" ++
    (outlierReport $ outlierEffect rfast) ++
    ")"
  putStrLn $ "Reporting slow with " ++
    (show $ pretty $ outlierEffect rslow) ++
    "% of outside effects ( " ++
    (outlierReport $ outlierEffect rslow) ++
    ")"
  putStrLn $ "Reporting string with " ++
    (show $ pretty $ outlierEffect rstring) ++
    "% of outside effects ( " ++
    (outlierReport $ outlierEffect rstring) ++
    ")"

  let hypList = [
        hypothesis "linear" hp,
        hypothesis "linearitmic" hpln,
        hypothesis "quadratic" hp2,
        hypothesis "cubic" hp3]

  putStrLn $ ("fastSort: " ++) $ pbest $ map fst $ testGroup rfast hypList (relevant)
  putStrLn $ ("slowSort: " ++) $ pbest $ map fst $ testGroup rslow hypList (relevant)
  putStrLn $ ("stringSort: " ++) $ pbest $ map fst $ testGroup rstring hypList (relevant)

  where pretty x = round $ x * 100
        pbest = intercalate ", "
