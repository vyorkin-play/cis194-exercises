module Main where

import Golf

main :: IO ()
main = do
  putStr $ histogram [1,1,1,5]
  putStrLn ""
  putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
  putStrLn ""
