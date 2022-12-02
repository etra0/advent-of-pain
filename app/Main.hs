module Main where

import System.Environment (getArgs)

import qualified AoP.Day01 as Day01
import qualified AoP.Day02 as Day02

main :: IO ()
main = do
  args <- getArgs

  case args of
    [input] -> case day of
      1 -> Day01.solve_all
      2 -> Day02.solve_all
      _ -> putStrLn "Unknown day"
      where day = read input
    _       -> putStrLn "Incorrect input"
