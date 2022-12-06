module Main where

import System.Environment (getArgs)

import qualified AoP.Day01 as Day01
import qualified AoP.Day02 as Day02
import qualified AoP.Day03 as Day03
import qualified AoP.Day04 as Day04
import qualified AoP.Day05 as Day05
import qualified AoP.Day06 as Day06

main :: IO ()
main = do
  args <- getArgs

  case args of
    [input] -> case day of
      1 -> Day01.solve_all
      2 -> Day02.solve_all
      3 -> Day03.solve_all
      4 -> Day04.solve_all
      5 -> Day05.solve_all
      6 -> Day06.solve_all
      _ -> putStrLn "Unknown day"
      where day = read input
    _       -> putStrLn "Incorrect input"
