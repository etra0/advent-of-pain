module AoP.Day01 where

import Data.List (sort)

inner_solve :: Integer -> [Integer] -> [String] -> [Integer]
inner_solve current_value current_list txt = 
  case txt of
    []           -> current_list
    ("" : rest)  -> inner_solve 0 (current_value : current_list) rest
    (x : rest) -> inner_solve ((read x) + current_value) current_list rest

last_n :: Integer -> [a] -> [a]
last_n val lst =
  case val of
    0   -> []
    _   -> (last lst) : last_n (val - 1) (init lst)

solve1 :: [String] -> Integer
solve1 = maximum . inner_solve 0 []

solve2 :: [String] -> Integer
solve2 = sum . last_n 3 . (sort . inner_solve 0 [])

solve_all :: IO ()
solve_all = do
  text_input <- readFile "inputs/input01.txt"
  print $ solve1 $ lines text_input
  print $ solve2 $ lines text_input

