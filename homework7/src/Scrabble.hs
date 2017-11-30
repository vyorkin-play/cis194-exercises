{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Monoid
import Data.Map (Map, fromList, findWithDefault)
import Data.Char (toLower)

scores :: Map Char Int
scores = fromList $
  [('a', 1), ('b', 3), ('c', 3), ('d', 2), ('e', 1), ('f', 4), ('g', 2)
  ,('h', 4), ('i', 1), ('j', 8), ('k', 5), ('l', 1), ('m', 3), ('n', 1)
  ,('o', 1), ('p', 3), ('q', 10), ('r', 1), ('s', 1), ('t', 1), ('u', 1)
  ,('v', 4), ('w', 4), ('x', 8), ('y', 4), ('z', 10)]

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score = Score . score0

score0 :: Char -> Int
score0 = flip (findWithDefault 0) scores . toLower

scoreString :: String -> Score
scoreString = Score . foldl1 (+) . map score0
