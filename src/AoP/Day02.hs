module AoP.Day02 where

data Throw
    = Rock
    | Paper
    | Scissor
    deriving (Show, Eq)

data Outcome
  = Win
  | Loss
  | Tie
  deriving (Show)

type Round = (Outcome, Throw)

is_winner :: (Throw, Throw) -> Round
is_winner (enemy, you)
  = let
      outcome = case (enemy, you) of
        (Rock, Scissor)  -> Loss
        (Paper, Rock)    -> Loss
        (Scissor, Paper) -> Loss
        _                ->
          if enemy == you then Tie
          else Win
    in
      (outcome, you)

parse_throw :: String -> Throw
parse_throw s = 
  case s of
    "A" -> Rock
    "B" -> Paper
    "C" -> Scissor
  -- Win choices:
    "X" -> Rock
    "Y" -> Paper
    "Z" -> Scissor
    _   -> error "bad input"

parse_match :: [String] -> (Throw, Throw)
parse_match [a, b] = (parse_throw a, parse_throw b)
parse_match _ = error "Wrong input"

calculate_score :: Round -> Int
calculate_score (outcome, throw) = 
  let throw_score = case throw of
        Rock     -> 1
        Paper    -> 2
        Scissor  -> 3
  in
    throw_score + case outcome of
        Win  -> 6
        Tie  -> 3
        Loss -> 0

solve1 :: String -> Int
solve1 s =
  let all_matches = map (parse_match . words) $ lines s
  in
    foldr (\ x y -> (calculate_score $ is_winner x) + y ) 0 all_matches
  
-- second part
parse_match2 :: [String] -> (Throw, Outcome)
parse_match2 [a, b] = (parse_throw a, parse_outcome b)
parse_match2 _ = error "Bad input"

parse_outcome :: String -> Outcome
parse_outcome s = 
  case s of
    "X" -> Loss
    "Y" -> Tie
    "Z" -> Win
    _ -> error "Bad input"

resolve_match2 :: (Throw, Outcome) -> Round
resolve_match2 (enemy_throw, outcome) =
  let
    throw = case (enemy_throw, outcome) of
       (Rock,      Win)   -> Paper
       (Rock,      Loss)  -> Scissor
       (Rock,      Tie)   -> Rock
       (Paper,     Tie)   -> Paper
       (Paper,     Win)   -> Scissor
       (Paper,     Loss)  -> Rock
       (Scissor,   Loss)  -> Paper
       (Scissor,   Tie)   -> Scissor
       (Scissor,   Win)   -> Rock
  in
       (outcome, throw)
    
solve2 :: String -> Int
solve2 s =
  let all_matches = map (calculate_score . resolve_match2 . parse_match2 . words) (lines s)
  in
    foldr (+) 0 all_matches

solve_all :: IO ()
solve_all = do
    text_input <- readFile "inputs/input02.txt"
    putStr "First case: "

    print $ solve1 text_input
    putStr "Second case: "
    print $ solve2 text_input
