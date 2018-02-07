module Birds
  ( run
  ) where

import           Control.Monad.Except

run :: IO ()
run = do
  print $ "bindRun    : " ++ printResult bindRun
  print $ "doRun      : " ++ printResult doRun
  print $ "failingRun : " ++ printResult failingRun

printResult :: Either String Pole -> String
printResult res =
  case res of
    Right pole -> "Yay! Pierre is still up and running! Birds on the pole : " ++ show pole
    Left e     -> "Oh no! Pierre fell! " ++ e

bindRun :: Either String Pole
bindRun = landLeft 2 (0, 0) >>= landRight 2 >>= landLeft 1

doRun :: Either String Pole
doRun = do
  first <- landLeft 2 (0, 0)
  second <- landRight 2 first
  landLeft 1 second

failingRun :: Either String Pole
failingRun = landLeft 2 (0, 0) >>= landRight 4 >>= landLeft 1

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n p = checkBirds n p "left" fst (\l -> (l, snd p))

landRight :: Birds -> Pole -> Either String Pole
landRight n p = checkBirds n p "right" snd (\r -> (fst p, r))

checkBirds :: Birds -> Pole -> String -> (Pole -> Birds) -> (Birds -> Pole) -> Either String Pole
checkBirds n p sideLabel side apply
  | abs newBirds < 4 = return (apply newBirds)
  | otherwise = throwError $ "There are " ++ show newBirds ++ " birds on the " ++ sideLabel ++ " side of the pole!"
  where
    newBirds = side p + n

banana :: Pole -> Maybe Pole
banana _ = Nothing
