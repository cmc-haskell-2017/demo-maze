module Main where

import System.Random

import Maze
import Maze.Space

main :: IO ()
main = do
  g <- newStdGen
  demo (genMaze (Area (-16, -16) (16, 16)) g)
