{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude

import           Control.Monad.ST
import qualified Data.Text           as T
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Mutable as MV

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
showSquare Blue  = '+'
showSquare Green = '='
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

setTile :: (Int, Int) -> tile -> Grid s tile -> ST s ()
setTile (irow, icol) tile rows = do
  row <- MV.unsafeRead rows irow
  MV.unsafeWrite row icol tile

getTile :: (Int, Int) -> Grid s tile -> ST s tile
getTile (irow, icol) rows = do
  row <- MV.unsafeRead rows irow
  MV.unsafeRead row icol

findTiles :: (tile -> Bool) -> Grid s tile -> ST s [(Int, Int)]
findTiles predicate rows = foldlM f [] indices
  where
    f accum irow = do
      row <- MV.unsafeRead rows irow
      let f' accum' icol = do
            tile <- MV.unsafeRead row icol
            if predicate tile
              then return $ (irow, icol) : accum'
              else return accum'
      (<> accum) <$> foldlM f' [] indices

data Move
  = Jump (Int, Int)
         (Int, Int)
  | Spawn (Int, Int)
  deriving (Show, Eq)

validMoves :: (Int, Int) -> BoardState s -> ST s [Move]
validMoves source@(irow, icol) rows = do
  spawns <- fmap Spawn <$> filterM emptyTile spawns_
  jumps <- fmap (Jump source) <$> filterM emptyTile jumps_
  return $ jumps <> spawns
  where
    emptyTile (irow, icol) =
      if irow < 0 || irow >= dim || icol < 0 || icol >= dim
        then return False
        else do
          tile <- getTile (irow, icol) rows
          return $ tile == Empty
    spawns_ =
      [ (irow - 1, icol - 1)
      , (irow - 1, icol)
      , (irow - 1, icol + 1)
      , (irow, icol - 1)
      , (irow, icol + 1)
      , (irow + 1, icol - 1)
      , (irow + 1, icol)
      , (irow + 1, icol + 1)
      ]
    jumps_ =
      [ (irow - 2, icol - 2)
      , (irow - 2, icol - 1)
      , (irow - 2, icol)
      , (irow - 2, icol + 1)
      , (irow - 2, icol + 2)
      , (irow - 1, icol - 2)
      , (irow - 1, icol + 2)
      , (irow, icol - 2)
      , (irow, icol + 2)
      , (irow + 1, icol - 2)
      , (irow + 1, icol + 2)
      , (irow + 2, icol - 2)
      , (irow + 2, icol - 1)
      , (irow + 2, icol)
      , (irow + 2, icol + 1)
      , (irow + 2, icol + 2)
      ]

main :: IO ()
main = do
  putStrLn "Hi!"
  board <- stToIO $ blankGrid Empty
  printBoard board
  stToIO $ do
    setTile (0, 0) Blue board
    setTile (6, 6) Blue board
    setTile (0, 6) Green board
    setTile (6, 0) Green board
  printBoard board
  blues <- stToIO $ findTiles (== Blue) board
  print blues
  moves <- stToIO $ validMoves (0, 0) board
  print moves
  where printBoard board = do
          putStrLn =<< stToIO (showGrid showSquare board)
          putStrLn ""
