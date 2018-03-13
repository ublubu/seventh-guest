{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude

import Control.Monad.ST
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as V
import qualified Data.Text as T

{- |
Grid UI:
- 7x7 space-separated characters
- modal interaction like vim
  - view-mode: +/=/_ tiles
  - select-mode: a-zA-z to select a tile to move, +/=/_ for enemy/empty tiles
  - move-mode: a-z to select an empty tile to move to

Game board implementation:
- Mutable 2D vector of blue/green/empty tiles

UI implementation:
- canvas: Mutable 2D vector of characters (tiles)
  - "paint" UI layers onto the canvas
- character input from stdin

-}

data Square = Blue | Green | Empty deriving (Show, Eq)
type Grid s tile = MV.MVector s (MV.MVector s tile)
type BoardState s = Grid s Square
type BoardUI s = Grid s Char

showSquare :: Square -> Char
showSquare Blue = 'X'
showSquare Green = 'O'
showSquare Empty = '_'

dim :: Int
dim = 7

indices :: [Int]
indices = [0..6]

showRow :: (tile -> Char) -> MV.MVector s tile -> ST s Text
showRow showTile squares = do
  items <- mapM (MV.unsafeRead squares) indices
  return . T.intercalate " " $ (T.singleton . showTile) <$> items

showGrid :: (tile -> Char) -> Grid s tile -> ST s Text
showGrid showTile rows = do
  items <- mapM (MV.unsafeRead rows) indices
  T.intercalate "\n" <$> mapM (showRow showTile) items

blankGrid :: tile -> ST s (Grid s tile)
blankGrid tile =
  MV.replicateM dim row
  where row = MV.replicate 7 tile

main :: IO ()
main = do
  putStrLn "Hi!"
  grid <- stToIO $ blankGrid Empty
  putStrLn =<< stToIO (showGrid showSquare grid)
