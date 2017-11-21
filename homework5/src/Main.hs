{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Ord (min, max)
import Control.Monad (liftM)

import ExprT
import Parser
import qualified StackVM as VM

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr = liftM eval <$> parseExp lit add mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
  lit = Mod7 . (`rem` 7)
  add (Mod7 x) (Mod7 y) = Mod7 $ (add x y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (mul x y) `mod` 7

instance Expr VM.Program where
  lit x = [VM.PushI x]
  add x y = x ++ y ++ [VM.Add]
  mul x y = x ++ y ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile s = (parseExp lit add mul s) :: Maybe VM.Program

reify :: ExprT -> ExprT
reify = id

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

main :: IO ()
main = do
  -- putStrLn . show $ evalStr "(2 + 3) * 4"
  putStrLn . show $ compile "(2 + 3) * 4"
