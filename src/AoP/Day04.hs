module AoP.Day04 where

-- Haskell doesn't have anything to split strings so...
split_by_char :: Char -> String -> [String]
split_by_char c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split_by_char c s''
    where (w, s'') = break (== c) s'

type Range = (Int, Int)
type Group = (Range, Range)

parse_range :: String -> Range
parse_range s
  = case (split_by_char '-' s) of
      [a, b] -> (read a, read b)
      _      -> error "Ill formed string"
  
parse_line :: String -> Group
parse_line inp
  = let
      spl = split_by_char ',' inp
    in
      case spl of
        [a, b] -> (parse_range a, parse_range b)
        _      -> error "Ill formed string"

check_contains :: Group -> Int
check_contains ((a, b), (c, d))
  = let
      (diff_a, diff_b) = (a - c, b - d)
    in
      if ((diff_a >= 0) && (diff_b <= 0)) || ((diff_a <= 0) && (diff_b >= 0)) then
         1
      else
         0

solve1 :: String -> Int
solve1 s = foldr (\x y -> y + (check_contains $ parse_line x)) 0 (lines s)

-- Part 2
check_contains2 :: Group -> Int
check_contains2 ((a, b), (c, d))
  | (a <= d) && (a >= c) = 1
  | (b <= d) && (b >= c) = 1
  | otherwise            = check_contains ((a, b), (c, d))

solve2 :: String -> Int
solve2 s = foldr (\x y -> y + (check_contains2 $ parse_line x)) 0 (lines s)

solve_all :: IO ()
solve_all = do
  text_input <- readFile "inputs/input04.txt"
  print $ solve1 text_input
  print $ solve2 text_input
