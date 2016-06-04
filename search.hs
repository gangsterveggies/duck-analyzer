import Duck

import Data.List
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Test.QuickCheck as QC

slowSearch :: ([Int], [Int]) -> [Bool]
slowSearch ([], _) = []
slowSearch ((x:xs), source) = fd : (slowSearch (xs, source))
  where fd = x `elem` source

fastSearch :: ([Int], [Int]) -> [Bool]
fastSearch (xs, source) = setSearch xs (Set.fromList source)

setSearch :: [Int] -> Set Int -> [Bool]
setSearch [] _ = []
setSearch (x:xs) s = fd : (setSearch xs s)
  where fd = x `elem` s

newtype OrdInt = OrdInt ([Int], [Int])

instance Sized OrdInt where
  dimSize (OrdInt a) = dimSize a

instance Cased OrdInt where
  genCase sz = do
    fs  <- QC.vectorOf nvec (QC.choose (1, 100))
    snd <- QC.vectorOf nvec (QC.choose (101, 1000))
    return (OrdInt (fs, snd))
    where nsingle = 2
          nvec = sz `div` nsingle

slowSearchWC :: OrdInt -> [Bool]
slowSearchWC (OrdInt a) = slowSearch a

headerText :: String
headerText = "\t-- Duck - Complexity Tester --\n\n" ++
             " Testing sorting algorithms\n" ++
             "   fastSearch:   search with haskell Set\n" ++
             "   slowSearch:   linear search in list (randomly generated)\n" ++
             "   slowSearchWC: linear search in list (worst case generated)\n"

main = do  
  let t = defaultParam {range = (100, 1000),
                        iterations = 15}

  putStrLn headerText

  rfast   <- runSingleTest t (fastSearch)
  rslow   <- runSingleTest t (slowSearch)
  rslowWC <- runSingleTest t (slowSearchWC)

  putStrLn ""
  putStrLn $ "Reporting fast with " ++
    (show $ pretty $ outlierEffect rfast) ++
    "% of outside effects"
  putStrLn $ "Reporting slow with " ++
    (show $ pretty $ outlierEffect rslow) ++
    "% of outside effects"
  putStrLn $ "Reporting slowWC with " ++
    (show $ pretty $ outlierEffect rslowWC) ++
    "% of outside effects"

  let hypList = [
        hypothesis "linear" hp,
        hypothesis "linearitmic" hpln,
        hypothesis "quadratic" hp2,
        hypothesis "cubic" hp3]

  putStrLn $ ("fastSearch: " ++)   $ pbest $ map fst $ testGroup rfast hypList
  putStrLn $ ("slowSearch: " ++)   $ pbest $ map fst $ testGroup rslow hypList
  putStrLn $ ("slowSearchWC: " ++) $ pbest $ map fst $ testGroup rslowWC hypList

  where pretty x = round $ x * 100
        pbest = intercalate ", "
