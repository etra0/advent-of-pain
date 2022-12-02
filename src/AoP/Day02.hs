module AoP.Day02 where

data Throw
    = Rock
    | Paper
    | Scissor
    deriving (Show)

data Outcome
  = Player
  | Enemy
  | Tie
  deriving (Show)

type Round = (Outcome, Throw)

parse_throw :: String -> Throw
parse_throw s = 
  case s of
    "A" -> Rock
    "B" -> Paper
    "C" -> Scissor
  -- Player choices:
    "X" -> Rock
    "Y" -> Paper
    "Z" -> Scissor
    _   -> error "bad input"

parse_match :: [String] -> (Throw, Throw)
parse_match [a, b] = (parse_throw a, parse_throw b)

parse_match _ = error "Wrong input"

resolve_match :: (Throw, Throw) -> Round
resolve_match throw = 
  let outcome = case throw of
       (Rock,      Paper)     -> Player
       (Rock,      Scissor)   -> Enemy
       (Rock,      Rock)      -> Tie
       (Paper,     Paper)     -> Tie
       (Paper,     Scissor)   -> Player
       (Paper,     Rock)      -> Enemy
       (Scissor,   Paper)     -> Enemy
       (Scissor,   Scissor)   -> Tie
       (Scissor,   Rock)      -> Player
  in
       (outcome, snd throw)

calculate_score :: Round -> Int
calculate_score (outcome, throw) = 
  let throw_score = case throw of
        Rock     -> 1
        Paper    -> 2
        Scissor  -> 3
  in
    throw_score + case outcome of
        Player -> 6
        Tie    -> 3
        Enemy  -> 0

parse_all_matches :: String -> [(Throw, Throw)]
parse_all_matches s = map parse_match $ map words $ lines s

solve1 :: String -> Int
solve1 s =
  let all_matches = parse_all_matches s
  in
    foldr (\ x y -> (calculate_score $ resolve_match x) + y ) 0 all_matches
  
-- second part
parse_match2 :: [String] -> (Throw, Outcome)
parse_match2 [a, b] = (parse_throw a, parse_outcome b)
parse_match2 _ = error "Bad input"

parse_outcome :: String -> Outcome
parse_outcome s = 
  case s of
    "X" -> Enemy
    "Y" -> Tie
    "Z" -> Player
    _ -> error "Bad input"

resolve_match2 :: (Throw, Outcome) -> Round
resolve_match2 (enemy_throw, outcome) =
  let throw = case (enemy_throw, outcome) of
       (Rock,      Player)  -> Paper
       (Rock,      Enemy)   -> Scissor
       (Rock,      Tie)     -> Rock
       (Paper,     Tie)     -> Paper
       (Paper,     Player)  -> Scissor
       (Paper,     Enemy)   -> Rock
       (Scissor,   Enemy)   -> Paper
       (Scissor,   Tie)     -> Scissor
       (Scissor,   Player)  -> Rock
  in
       (outcome, throw)
    
solve2 :: String -> Int
solve2 s =
  let all_matches = map calculate_score $ map resolve_match2 $ map parse_match2 $ map words $ lines s
  in
    foldr (+) 0 all_matches

solve_all :: IO ()
solve_all = do
    text_input <- readFile "inputs/input02.txt"
    putStr "First case: "

    print $ solve1 text_input
    putStr "Second case: "
    print $ solve2 text_input
