module AoP.Day06 where

check_belongs :: String -> Bool
check_belongs (x:xs)
  | x `elem` xs  = True
  | otherwise    = check_belongs xs
check_belongs [] = False

solver :: Int -> Int -> String -> Int
solver spl n ls
  = let
      h = take spl ls
    in
      if (not $ check_belongs h) then
        n + spl
      else
        solver spl (n + 1) (tail ls)

solve1 :: String -> Int
solve1 = solver 4 0

solve2 :: String -> Int
solve2 = solver 14 0

solve_all :: IO ()
solve_all = do
  text_input <- readFile "inputs/input06.txt"
  print $ solve1 text_input
  print $ solve2 text_input
