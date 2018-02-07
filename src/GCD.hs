module GCD where

import           Control.Monad.Writer

run :: IO ()
run = mapM_ print $ fromDiffList $ snd $ runWriter $ reverseGcd 38 14

newtype DiffList a = DiffList
  { getDiffList :: [a] -> [a]
  }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> xs ++ [])
  (DiffList f) `mappend` (DiffList g) = DiffList (f . g)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)

reverseGcd :: Int -> Int -> Writer (DiffList String) Int
reverseGcd a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    result <- reverseGcd b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result
