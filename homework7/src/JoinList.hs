{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid ((<>))

import Buffer
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

append :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
append l r = Append ((tag l) <> (tag r)) l r

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = append

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(!!?) :: [a] -> Int -> Maybe a
[]      !!? _         = Nothing
_       !!? i | i < 0 = Nothing
(x:xs)  !!? 0         = Just x
(x:xs)  !!? i         = xs !!? (i - 1)

list :: JoinList m a -> [a]
list Empty = []
list (Single _ a) = [a]
list (Append _ l r) = list l ++ list r

indexJ' :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ' i jl = (list jl) !!? i

-- unbalanced:
-- ===============================
--
--                12
--         7              5
--    4         3    4          1
--    ...........................
--
-- balanced:
-- ===============================
--
--                12
--         5              7
--    2         3    3         4
--    ...........................
--
--
--    indexJ:
--
--    i = 8
--    i = 3
--
--    sp = sl + sr
--    i >  sp -> Nothing
--    i <  sl -> indexJ i l
--    i >= sl -> indexJ (i - sl) r
--

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ (Single _ x) = Just x
indexJ i (Append sp l r) | i < (size' sp) && i >= 0 =
  case compare i sl of
    LT -> indexJ i l
    _ -> indexJ (i - sl) r
  where sl = sizeJ l
indexJ _ _ = Nothing

-- drop' :: Int -> [a] -> [a]
-- drop' n _ | n <= 0 = []
-- drop' _ [] = []
-- drop' n (_:xs) = drop (n - 1) xs

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n l | n <= 0 = l
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l r) =
  let sl = sizeJ l
      sr = sizeJ r
  in case compare n sl of
    LT -> dropJ (n - sr) l
    _ -> dropJ (n - sl) r

-- take' :: Int -> [a] -> [a]
-- take' n _ | n <= 0 = []
-- take' _ [] = []
-- take' n (x:xs) = x : take' (n - 1) xs

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n l | n <= 0 = Empty
takeJ _ l@(Single _ _) = l
takeJ n (Append _ l r) =
  case compare n sl of
    GT -> append l $ takeJ (n - sl) r
    _ -> takeJ n l
  where sl = sizeJ l

sizeJ :: (Sized b) => JoinList b a -> Int
sizeJ Empty = 0
sizeJ (Single s _) = size' s
sizeJ (Append s _ _) = size' s

size' :: (Sized b) => b -> Int
size' = getSize . size

scoreLine :: String -> JoinList Score String
scoreLine "" = Empty
scoreLine s = Single (scoreString s) s

lineFromString :: String -> JoinList (Score, Size) String
lineFromString "" = Empty
lineFromString s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ l r) = toString l ++ toString r
  fromString s = foldl (\acc l -> acc +++ lineFromString l) Empty . lines $ s
  line = indexJ
  replaceLine n s b | n >= 0 = takeJ (n - 1) b +++ lineFromString s +++ dropJ n b
  replaceLine _ _ b = b
  numLines Empty = 0
  numLines (Single _ _) = 1
  numLines (Append _ l r) = numLines l + numLines r
  value Empty = 0
  value (Single (Score n, _) _) = n
  value (Append (Score n, _) _ _) = n
