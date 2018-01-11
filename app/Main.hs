module Main where

import RoadResolver
import Monoid

main :: IO ()
main = execLengthCompare
-- main = putStrLn $ concatMap (show . fst) (optimalPath heathrowToLondon)
