module AoP.Day05 where


type Crate = Char
type Stack = [Crate]

-- Instruction is move N from A to B
type Instruction = (Int, Int, Int)

get_stacks :: String -> Int
get_stacks s = ((length s) + 1) `div` 4

split_input :: [String] -> (String, String)
split_input s
  = let
      (a, b) = break (== "") s
      reduce = foldr (\x y -> (x ++ "\n") ++ y) ""
    in
      (reduce a, reduce b)

convert_line_to_stack :: Int -> [String] -> Int -> Stack
convert_line_to_stack l (s:xs) ix
  = let
      char = s !! ((ix * 4) + 1)
    in
      if not ((s !! 1) == '1') then
        case char of
          ' '       -> convert_line_to_stack l xs ix
          _         -> char : convert_line_to_stack l xs ix
      else
        []
convert_line_to_stack _ [] _ = []

parse_stack :: String -> [Stack]
parse_stack s
  = let
      splitted = lines s
      n_stacks = get_stacks $ head splitted
    in
      [convert_line_to_stack n_stacks splitted i | i <- [0..n_stacks - 1]]

parse_instruction :: String -> Instruction
parse_instruction instr
  = let w = words instr
    in (read (w !! 1), read (w !! 3), read (w !! 5))

-- This incredibly ugly but it's what I had come with.
evaluate_instruction :: Instruction -> [Stack] -> [Stack]
evaluate_instruction (n, from, to) s
  | n > 0                           
    = let
        (x1:xs1) = s !! (from - 1)
        smth = \x i -> if i == (from - 1) then xs1 else if i == (to - 1) then x1:x else x
        s' = zipWith smth s [0..]
      in
        evaluate_instruction ((n - 1), from, to) s'
  | otherwise = s

tsolve1 :: [Instruction] -> [Stack] -> [Stack]
tsolve1 (x:xs) s = tsolve1 xs (evaluate_instruction x s)
tsolve1 [] s = s

solve1 :: String -> IO ()
solve1 inp = do
  let (stacks, instr) = split_input $ lines inp
  let parsed_instr = map parse_instruction $ lines $ dropWhile (== '\n') instr
  let parsed_stack = parse_stack stacks
  print $ map head $ tsolve1 parsed_instr parsed_stack

-- Part 2.

evaluate_instruction2 :: Instruction -> [Stack] -> [Stack]
evaluate_instruction2 (n, from, to) s
  = let
      (h, t) = splitAt n (s !! (from - 1))
      inplace = (\x i -> if i == (from - 1) then t else if i == (to - 1) then (h ++ x) else x)
    in
      zipWith inplace s [0..]

tsolve2 :: [Instruction] -> [Stack] -> [Stack]
tsolve2 (x:xs) s = tsolve2 xs (evaluate_instruction2 x s)
tsolve2 [] s = s

solve2 :: String -> IO ()
solve2 inp = do
  let (stacks, instr) = split_input $ lines inp
  let parsed_instr = map parse_instruction $ lines $ dropWhile (== '\n') instr
  let parsed_stack = parse_stack stacks
  print $ map head $ tsolve2 parsed_instr parsed_stack

solve_all :: IO ()
solve_all = do
  inp <- readFile "inputs/input05.txt"
  solve1 inp
  solve2 inp
