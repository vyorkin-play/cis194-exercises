module Main where

import Data.Function (on)
import Data.List (sortBy, (\\))
import Data.Ord (comparing)
import Data.Foldable (foldr', maximumBy)
-- my reasonings about fold lazyness
import Crap

fun1 :: [Integer] -> Integer
fun1 = foldr (\x y -> if even x then y * (x - 2) else y) 1

-- fun2(3)
-- fun2(3 * 3 + 1)
-- 10 + fun2(10 / 2)
-- 10 + fun2(3 * (10 / 2) + 1)
-- 10 + fun2(16)
-- 10 + (16 + fun2(8))
-- 10 + (16 + (8 + fun2(4)))
-- 10 + (16 + (8 + (4 + fun2(2)))
-- 10 + (16 + (8 + (4 + (2 + fun2(1)))
-- 10 + (16 + (8 + (4 + (2 + 0))
-- 40

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n | even n = n + fun2' (n `div` 2)
        | otherwise = fun2' (3 * n + 1)

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate f
  where f x = if even x then x `div` 2 else 3 * x + 1

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr' insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h' left x' right) =
  let lh = height left
      rh = height right
  in case compare lh rh of
       GT -> Node (lh + 1) left x' (insert x right)
       _ -> Node (rh + 1) (insert x left) x' right
  where
    height (Node h'' _ _ _) = h''
    height Leaf = 0

xor :: [Bool] -> Bool
xor = odd . length . filter (== True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x z  -> (f x) : z) []

-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let elimination = filter (<= n) [i + j + 2 * i * j | i <- [1..n], j <- [i..n]]
  in map (\x -> x * 2 + 1) $ [1..n] \\ elimination

main :: IO ()
main = do
  putStrLn . show $ sieveSundaram 100
  -- putStrLn . show $ map' (*2) [1..5]
  -- putStrLn . show $ xor [False, True, False]
  -- putStrLn . show $ xor [False, True, False, False, True]
  -- putStrLn . show $ foldTree "ABCDEFGHIJ"
  -- putStrLn . show $ fun2' 7
  -- putStrLn . show $ fun2 7
  -- putStrLn . show $ vfoldr (+) 0 [1..3]
  -- putStrLn . show $ vfoldl' (+) 0 [1..1000000]

