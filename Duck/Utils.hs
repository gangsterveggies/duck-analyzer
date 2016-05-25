module Duck.Utils where

putStrIf :: Bool -> String -> IO()
putStrIf False _ = return()
putStrIf True str = putStrLn str
