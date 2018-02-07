module ThreeCoins
  ( run
  ) where

import           Control.Monad.State
import           System.Random

run :: IO ()
run = do
  putStrLn $ "threeCoins     : " ++ show (runState threeCoins (mkStdGen 33))
  putStrLn $ "threeCoinsBind : " ++ show (runState threeCoins (mkStdGen 33))

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  c1 <- randomSt
  c2 <- randomSt
  c3 <- randomSt
  return (c1, c2, c3)

threeCoinsBind :: State StdGen (Bool, Bool, Bool)
threeCoinsBind = randomSt >>= \c1 -> randomSt >>= \c2 -> randomSt >>= \c3 -> return (c1, c2, c3)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random
