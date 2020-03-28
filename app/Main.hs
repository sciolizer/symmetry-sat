module Main where

import System.TimeIt

import Dimacs

main :: IO ()
main = do
  putStrLn "temporarily disabled" {-
  c <- getContents
  let cls = parseDimacs c
  putStrLn $ show (length cls) ++ " clauses" -- 'length' forces a parse to the end so we can start timing things
  r <- timeIt $ do
    let res = search cls
    case res of
      Nothing -> putStrLn "UNSAT" >> return Nothing
      Just ans -> putStr "SAT: " >> return (Just ans)
  case r of
    Just sol -> print $ all (satisfies sol) cls
    Nothing -> return ()
-}