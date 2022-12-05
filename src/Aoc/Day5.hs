module Aoc.Day5 (aoc) where

import Data.List (transpose)
import Data.List.Split

-- Safely gets the element at index, if not returns an '-'
(!!!) :: String -> Int -> Char
(!!!) line index = if length line >= index then line !!!! index else '-'
  where
    (!!!!) l i =
      case l !! i of
        ' ' -> '-'
        x -> x

-- Dirtily gets characters at every 4th position using a texas range
extractLetters :: String -> [Char]
extractLetters line = [line !!! index | index <- [1, 5 .. 34]]

createCommandTriple :: [String] -> [(Int, Int, Int)]
createCommandTriple xs = [splitToTriple line | line <- xs]
  where
    splitToTriple :: String -> (Int, Int, Int)
    splitToTriple line = numberifyTriple $ triple $ splitOn " " line
    triple [_, a, _, b, _, c] = (a, b, c)
    triple _ = error "Not a triple"
    numberifyTriple (a, b, c) = (read a, read b, read c)

execute :: [(Int, Int, Int)] -> [String] -> [String]
execute [] stack = stack
execute ((count, from, to) : commands) stack = execute commands updatedStack
  where
    lineToMoveFrom = stack !! (from - 1)
    itemsToMove = reverse $ drop (length lineToMoveFrom - count) lineToMoveFrom
    updatedStack =
      [ if index == from
          then reverse $ drop count $ reverse lineToMoveFrom
          else
            if index == to
              then line ++ itemsToMove
              else line
        | (line, index) <- zip stack [1 ..]
      ]

executePart2 :: [(Int, Int, Int)] -> [String] -> [String]
executePart2 [] stack = stack
executePart2 ((count, from, to) : commands) stack = executePart2 commands updatedStack
  where
    lineToMoveFrom = stack !! (from - 1)
    itemsToMove = drop (length lineToMoveFrom - count) lineToMoveFrom
    updatedStack =
      [ if index == from
          then reverse $ drop count $ reverse lineToMoveFrom
          else
            if index == to
              then line ++ itemsToMove
              else line
        | (line, index) <- zip stack [1 ..]
      ]

topCrates :: [String] -> String
topCrates = map safeLast
  where
    safeLast :: String -> Char
    safeLast "" = ' '
    safeLast x = last x

part1 :: [String] -> String
part1 input = show $ topCrates $ execute commands initialStack
  where
    initialStack = map (filter (`notElem` "-")) $ transpose $ map extractLetters $ reverse $ take 8 input
    commands = createCommandTriple $ drop 10 input

part2 :: [String] -> String
part2 input = show $ topCrates $ executePart2 commands initialStack
  where
    initialStack = map (filter (`notElem` "-")) $ transpose $ map extractLetters $ reverse $ take 8 input
    commands = createCommandTriple $ drop 10 input

aoc :: IO ()
aoc = do
  readFile "aoc-data/day5.txt" >>= putStrLn . part1 . lines
  readFile "aoc-data/day5.txt" >>= putStrLn . part2 . lines
