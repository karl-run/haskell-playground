module Aoc.Day1 (aoc) where

import Data.List (elemIndex, sort)

splitAtEmptyLine :: [String] -> [[String]]
splitAtEmptyLine items =
  case itemIndexMaybe of
    Just itemIndex -> [take itemIndex items] ++ (splitAtEmptyLine (drop (itemIndex + 1) items))
    Nothing -> []
  where
    itemIndexMaybe = "" `elemIndex` items

numberify :: [[String]] -> [[Int]]
numberify = map (map read)

calculateCalories :: [String] -> [Int]
calculateCalories = map sum . (numberify . splitAtEmptyLine)

processInput :: [String] -> String
processInput input = show (sum (drop (length sortedItems - 3) sortedItems))
  where
    sortedItems = sort (calculateCalories input)

aoc :: IO ()
aoc = do
  readFile "aoc-data/day1.txt" >>= putStrLn . processInput . lines
