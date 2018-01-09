module Main where

import RoadResolver

main :: IO ()
main = runOptimalPath
-- main = putStrLn $ concatMap (show . fst) (optimalPath heathrowToLondon)
