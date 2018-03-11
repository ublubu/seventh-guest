{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude

import Control.Monad.ST
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as V
import qualified Data.Text as T

data Square = Blue | Green | Empty deriving (Show, Eq)

type Grid s = MV.MVector s (MV.MVector s Square)

showSquare :: Square -> Text
showSquare Blue = "X"
showSquare Green = "O"
showSquare Empty = "_"

dim :: Int
dim = 7

indices :: [Int]
indices = [0..6]

showRow :: MV.MVector s Square -> ST s Text
showRow squares = do
  items <- mapM (MV.unsafeRead squares) indices
  return . T.intercalate " " $ showSquare <$> items

showGrid :: Grid s -> ST s Text
showGrid rows = do
  items <- mapM (MV.unsafeRead rows) indices
  T.intercalate "\n" <$> mapM showRow items

emptyGrid :: ST s (Grid s)
emptyGrid =
  MV.replicateM dim row
  where row = MV.replicate 7 Empty

main :: IO ()
main = do
  putStrLn "Hi!"
  grid <- stToIO emptyGrid
  putStrLn =<< stToIO (showGrid grid)
