module RoadResolver
  ( runOptimalPath
  ) where

type RoadSystem = [Section]

type Path = [(Label, Int)]

data Section = Section {getA :: Int, getB :: Int, getC :: Int}

data Label = A | B | C deriving (Show)

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0
                   ]

runOptimalPath :: IO ()
runOptimalPath = do
    (path, pathTime) <- optimalPathFromInput <$> getContents
    putStrLn $ "The best path to take is: " ++ path
    putStrLn $ "Time taken: " ++ show pathTime

optimalPathFromInput :: String -> (String, Int)
optimalPathFromInput = (\path -> (concatMap (show . fst) path, pathSum path))
  . optimalPath
  . map (\[a, b, c] -> Section a b c)
  . groupsOf 3
  . map read
  . lines

optimalPath :: RoadSystem -> Path
optimalPath roads = reverse
  $ let (pathA, pathB) = foldl step ([],[]) roads
    in if pathSum pathA > pathSum pathB
       then pathB
       else pathA


step :: (Path, Path) -> Section -> (Path, Path)
step (pathA, pathB) (Section a b c) =
  let sumA = pathSum pathA
      sumB = pathSum pathB
      nextAfromA = sumA + a
      nextAfromB = sumB + b + c
      nextBfromA = sumA + a + c
      nextBfromB = sumB + b
      nextA = if nextAfromA <= nextAfromB
              then (A,a):pathA
              else (C,c):(B,b):pathB
      nextB = if nextBfromA <= nextBfromB
              then (C,c):(A,a):pathA
              else (B,b):pathB
   in (nextA, nextB)

pathSum :: Path -> Int
pathSum = sum . map snd

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
