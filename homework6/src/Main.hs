module Main where

import Fibonacci

main :: IO ()
main = do
  putStrLn . show $ take 30 fibs2
