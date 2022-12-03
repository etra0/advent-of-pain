module AoP.Day03 where

import qualified Data.Set as Set
import Data.Char (ord)

split_half :: String -> [String]
split_half s =
    let
        (a, b) = splitAt ((length s) `div` 2) s
    in
        [a, b]

calculate_score :: Char -> Int
calculate_score c
    | (ord c) >= (ord 'a') = (ord c) - (ord 'a') + 1
    | otherwise            = (ord c) - (ord 'A') + 27

get_shared :: [String] -> Char
get_shared lst = 
    let
        lists_as_sets = map Set.fromList lst
    in
        Set.elemAt 0 $ foldr Set.intersection (head lists_as_sets) lists_as_sets

solve1 :: String -> Int
solve1 inp = foldr (+) 0 (map calculate_score $ map get_shared $ map split_half $ lines inp)

middle :: [String] -> [String] -> Int
middle curr [] = calculate_score $ get_shared curr
middle curr next = 
    let
        (a, b) = splitAt 3 next
    in
        (middle a b) + (calculate_score $ get_shared curr)

solve2 :: String -> Int
solve2 inp =
    let
        (a, b) = splitAt 3 $ lines inp
    in
        middle a b

solve_all :: IO ()
solve_all = do
    text_input <- readFile "inputs/input03.txt"
    print $ solve1 text_input
    print $ solve2 text_input
