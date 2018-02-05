module Birds
  ( run
  ) where

run :: IO ()
run = do
  print $ "bindRun = " ++ show bindRun
  print $ "doRun = " ++ show doRun
  return ()

bindRun :: Maybe Pole
bindRun = landLeft 2 (0, 0) >>= landRight 2 >>= landLeft 1

doRun :: Maybe Pole
doRun = do
  first <- landLeft 2 (0, 0)
  second <- landRight 2 first
  landLeft 1 second

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n p = checkBirds n p fst (\l -> (l, snd p))

landRight :: Birds -> Pole -> Maybe Pole
landRight n p = checkBirds n p snd (\r -> (fst p, r))

checkBirds :: Birds -> Pole -> (Pole -> Birds) -> (Birds -> Pole) -> Maybe Pole
checkBirds n p side apply
  | abs newBirds < 4 = Just (apply newBirds)
  | otherwise = Nothing
  where
    newBirds = side p + n

banana :: Pole -> Maybe Pole
banana _ = Nothing
