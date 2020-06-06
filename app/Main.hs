{-# language TemplateHaskell #-}
module Main where

import Data.Deriving (deriveEq1, deriveOrd1)
import Data.Functor.Classes (compare1, eq1)
import Data.Void (Void)

data T a
  = A
  | B Int
  | C Int
  | D
  | E Int
  | F
deriveEq1 ''T
deriveOrd1 ''T
instance Eq a => Eq (T a) where; (==) = eq1
instance Ord a => Ord (T a) where; compare = compare1

main :: IO ()
main =
  let
    x = E 0 :: T Void
  in
    print $ compare x x
