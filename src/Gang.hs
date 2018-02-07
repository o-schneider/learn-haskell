module Gang
  ( run
  ) where

import           Data.Monoid (Sum)

type Food = String

type Price = Sum Int

run :: IO ()
run = do
  print $ (3, "Smallish gang.") `applyLog` isBigGang
  print $ (30, "A freaking platoon.") `applyLog` isBigGang
  print $ ("beans", 10) `applyLog` addDrink
  print $ ("jerky", 25) `applyLog` addDrink
  print $ ("dogmeat", 5) `applyLog` addDrink
  print $ ("dogmeat", 5) `applyLog` addDrink `applyLog` addDrink

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", 25)
addDrink "jerky" = ("whiskey", 99)
addDrink _       = ("beer", 30)

applyLog :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f =
  let (y, newLog) = f x
  in (y, log `mappend` newLog)
