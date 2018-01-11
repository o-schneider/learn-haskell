module Monoid
  ( execLengthCompare
  )
  where

import Data.Monoid

execLengthCompare :: IO ()
execLengthCompare = do
   (first, second) <- (,)
     <$> prompt "Please type in the first word :"
     <*> prompt "Thanks ! And now, the second please : "
   putStrLn $ show (lengthCompare first second)

prompt :: String -> IO (String)
prompt s = do
  putStrLn s
  x <- getLine
  return x

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)
