module Main where

import RoadResolver

main :: IO ()
main = putStrLn $ concatMap (show . fst) (optimalPath heathrowToLondon)
