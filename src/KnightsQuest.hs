module KnightsQuest
  ( runKnightsQuest
  ) where

import           Control.Monad

runKnightsQuest :: IO ()
runKnightsQuest = print $ howToReachIn3 (6, 2) (6, 3)

type KnightPos = (Int, Int)

howToReachIn3 :: KnightPos -> KnightPos -> [(KnightPos, KnightPos, KnightPos)]
howToReachIn3 pos end = filter (\(_, _, p3) -> end == p3) (scanIn3 pos)

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 pos end = end `elem` in3 pos

in3 :: KnightPos -> [KnightPos]
in3 pos = moveKnight pos >>= moveKnight >>= moveKnight

scanIn3 :: KnightPos -> [(KnightPos, KnightPos, KnightPos)]
scanIn3 pos = moveKnight pos >>= \p1 -> moveKnight p1 >>= \p2 -> moveKnight p2 >>= \p3 -> return (p1, p2, p3)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (x, y) =
  filter
    isOnBoard
    [ (x + 2, y + 1)
    , (x + 2, y - 1)
    , (x - 2, y + 1)
    , (x - 2, y - 1)
    , (x + 1, y + 2)
    , (x + 1, y - 2)
    , (x - 1, y + 2)
    , (x - 1, y - 2)
    ]

isOnBoard :: KnightPos -> Bool
isOnBoard (x, y) = onBoard x && onBoard y
  where
    onBoard x = x `elem` [1 .. 8]
