module Crap where

vfoldr :: (a -> b -> b) -> b -> [a] -> b
vfoldr _ z [] = z
vfoldr f z (x:xs) = f x $ vfoldr f z xs

vfoldl :: (b -> a -> b) -> b -> [a] -> b
vfoldl _ z [] = z
vfoldl f z (x:xs) = let z' = f z x
                    in vfoldl f z' xs

(?) :: Int -> Int -> Int
_ ? 0 = 0
_ ? y = y

list :: [Int]
list = [2, 3, undefined, 5, 0]

okey = vfoldl (?) 1 list
boom = vfoldl' (?) 1 list

vfoldl' :: (b -> a -> b) -> b -> [a] -> b
vfoldl' _ z [] = z
vfoldl' f z (x:xs) = let z' = f z x
                     in seq z' $ vfoldl' f z' xs
