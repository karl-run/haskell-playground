module Aoc.Day6 (aoc) where

import Data.Set (fromList, toList)

indexOfFirstUnique :: Int -> String -> Int -> Int
indexOfFirstUnique _ "" _ = -1
indexOfFirstUnique uniqueWidth xs idx =
  if unique (take uniqueWidth xs)
    then idx
    else indexOfFirstUnique uniqueWidth (tail xs) (idx + 1)
  where
    unique :: String -> Bool
    unique x = length (toList (fromList x)) == length x

indexOfFirst4Unique :: String -> Int -> Int
indexOfFirst4Unique = indexOfFirstUnique 4

indexOfFirst14Unique :: String -> Int -> Int
indexOfFirst14Unique = indexOfFirstUnique 14

part1 :: [String] -> String
part1 input = show $ (+) 4 $ indexOfFirst4Unique (head input) 0

part2 :: [String] -> String
part2 input = show $ (+) 14 $ indexOfFirst14Unique (head input) 0

aoc :: IO ()
aoc = do
  readFile "aoc-data/day6.txt" >>= putStrLn . part1 . lines
  readFile "aoc-data/day6.txt" >>= putStrLn . part2 . lines
