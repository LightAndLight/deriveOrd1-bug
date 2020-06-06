module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.List as List
import System.Process

{-

{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Main where

import Data.Deriving (deriveEq1, deriveOrd1)
import Data.Functor.Classes (compare1, eq1)
import Data.Void (Void)

data T a
  = A a
  | B a
  | D
  | E
  | F
  | G Int
deriveEq1 ''T
deriveOrd1 ''T
instance Eq a => Eq (T a) where; (==) = eq1
instance Ord a => Ord (T a) where; compare = compare1

main :: IO ()
main =
  let
    x = G 0 :: T Void
  in
    print $ compare x x

-}

data Name = A | B | C | D | E | F | G | H | I | J | K | L
  deriving (Eq, Show)

data Ctor = Ctor { name :: Name, hasArg :: Bool }
  deriving Show

file :: [Ctor] -> Ctor -> String
file ctors target =
  unlines
  [ "{-# language TemplateHaskell #-}"
  , "module Main where"
  , ""
  , "import Data.Deriving (deriveEq1, deriveOrd1)"
  , "import Data.Functor.Classes (compare1, eq1)"
  , "import Data.Void (Void)"
  , ""
  , "data T a = " <>
    fold
      (List.intersperse " | " $
       fmap
         (\c -> show (name c) <> if hasArg c then " Int" else "")
         ctors
      )
  , "deriveEq1 ''T"
  , "deriveOrd1 ''T"
  , "instance Eq a => Eq (T a) where; (==) = eq1"
  , "instance Ord a => Ord (T a) where; compare = compare1"
  , ""
  , "main :: IO ()"
  , "main ="
  , "  let"
  , "    x = " <>
    show (name target) <>
    (if hasArg target then " 0" else "") <>
    " :: T Void"
  , "  in"
  , "    print $ compare x x"
  ]

genName :: [Name] -> Gen Name
genName seen =
  Gen.filter (`notElem` seen) $
  Gen.element [A, B, C, D, E, F, G, H, I, J, K, L]


genCtors :: [Name] -> Int -> Gen [Ctor]
genCtors seen 0 = pure []
genCtors seen n = do
  name <- genName seen
  (:) <$>
    (Ctor name <$> Gen.bool) <*>
    genCtors (name:seen) (n-1)

main :: IO Bool
main =
  check . withShrinks 500 . withTests 300 . property $ do
    n <- forAll $ Gen.int (Range.constant 1 6)
    ctors <- forAll $ genCtors [] n
    target <- forAll $ Gen.element ctors
    res <- liftIO $ do
      writeFile "app/Main.hs" (file ctors target)
      callProcess "cabal" ["new-build", "deriveOrd1-bug"]
      readProcess "cabal" ["new-run", "deriveOrd1-bug"] ""
    res === "Up to date\nEQ\n"
