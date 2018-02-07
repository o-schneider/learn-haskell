module ReaderTest
  ( run
  ) where

import           Control.Monad.Reader

run :: IO ()
run = do
  print $ addStuff 3
  print $ addStuff' 3

addStuff :: Int -> Int
addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)

addStuff' :: Int -> Int
addStuff' = (* 2) >>= \a -> (+ 10) >>= \b -> return (a + b)
