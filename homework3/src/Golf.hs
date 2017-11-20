module Golf where

import Data.List (nub, intercalate)
import Data.List.Split (divvy)

skips :: [a] -> [[a]]
skips xs = map (every xs) [1..length xs]

every :: [a] -> Int -> [a]
every xs k = [xs !! i | i <- [k - 1, 2 * k - 1..length xs - 1]]

localMaxima :: [Integer] -> [Integer]
localMaxima = map (!! 1) . filter (\[a, b, c] -> b > a && b > c) . divvy 3 1

histogram :: [Integer] -> String
histogram xs = intercalate "\n" $ graph ++ [axis, nums]
  where
    graph = [[draw r c | c <- [0..9]] | r <- [rows, rows - 1..0]]
    draw r c = if (freq !! c) > r then '*' else ' '
    rows = maximum freq
    freq = [count x xs | x <- nub [0..9]]
    count x = length . filter (x ==)
    axis = concat . replicate 10 $ "="
    nums = concatMap show [0..9]
