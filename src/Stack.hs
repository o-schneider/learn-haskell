module Stack
  ( run
  ) where

import           Control.Monad.State

run :: IO ()
run = do
  putStrLn "-- pop "
  putStrLn $ "pop      : " ++ show (runState pop [5, 8])
  putStrLn $ "popState : " ++ show (runState popState [5, 8])
  putStrLn $ "popDo    : " ++ show (runState popDo [5, 8])
  putStrLn "-- push "
  putStrLn $ "push      : " ++ show (runState (push 5) [])
  putStrLn $ "pushState : " ++ show (runState (pushState 5) [])
  putStrLn $ "pushDo    : " ++ show (runState (pushDo 5) [])
  putStrLn "-- stackManip "
  putStrLn $ "stackManip     : " ++ show (runState stackManip [5, 8, 2, 1])
  putStrLn $ "stackManipBind : " ++ show (runState stackManipBind [5, 8, 2, 1])
  putStrLn "-- stackStuff "
  putStrLn $ "stackStuff     : " ++ show (runState stackStuff [9, 0, 2, 1, 0])
  putStrLn $ "stackStuffBind : " ++ show (runState stackStuff [9, 0, 2, 1, 0])

type Stack a = [a]

stackManip :: State (Stack Int) Int
stackManip = do
  push 3
  pop
  pop

stackManipBind :: State (Stack Int) Int
stackManipBind = push 3 >>= const pop >>= const pop

stackStuff :: State (Stack Int) ()
stackStuff = do
  a <- pop
  if a == 5
    then push a
    else do
      push 3
      push 8

stackStuffBind :: State (Stack Int) ()
stackStuffBind =
  pop >>=
  (\a ->
     if a == 5
       then push a
       else push 3 >>= const (push 8))

pop :: State (Stack a) a
pop = get >>= \(x:xs) -> put xs >> return x

popState :: State (Stack a) a
popState = state $ \(x:xs) -> (x, xs)

popDo :: State (Stack a) a
popDo = do
  (x:xs) <- get
  put xs
  return x

push :: a -> State (Stack a) ()
push x = get >>= \xs -> put (x : xs)

pushState :: a -> State (Stack a) ()
pushState x = state $ \xs -> ((), x : xs)

pushDo :: a -> State (Stack a) ()
pushDo x = do
  xs <- get
  put (x : xs)
