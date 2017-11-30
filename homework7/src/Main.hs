module Main where

import Buffer
import StringBuffer
import Editor
import JoinList
import Sized
import Scrabble

txt :: String
txt = unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

buf :: JoinList (Score, Size) String
buf = fromString txt

main = runEditor editor buf
-- main = runEditor editor txt
