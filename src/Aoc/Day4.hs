module Aoc.Day4 (aoc) where

import Data.List.Split

tuple :: [String] -> (String, String)
tuple [a, b] = (a, b)
tuple x = error ("Not a tuple" ++ show x)

splitToTuple :: String -> String -> (String, String)
splitToTuple delim value = tuple $ splitOn delim value

toRangeTuples :: String -> ((String, String), (String, String))
toRangeTuples line = (splitToTuple "-" left, splitToTuple "-" right)
  where (left, right) = splitToTuple "," line

numberify :: ((String, String), (String, String)) -> ((Int, Int), (Int, Int))
numberify ((lFst, lSnd), (rFst, rSnd)) = ((read lFst, read lSnd), (read rFst, read rSnd))

containedInAny :: ((Int, Int), (Int, Int)) -> Bool
containedInAny (left, right) = left `containedIn` right || right `containedIn` left
  where containedIn (lFst, lSnd) (rFst, rSnd) = lFst >= rFst && lSnd <= rSnd

overlaps :: ((Int, Int), (Int, Int)) -> Bool
overlaps ((lFst, lSnd), (rFst, rSnd)) = lFst <= rSnd && lSnd >= rFst

part1 :: [String] -> String
part1 input = show $ length $ filter containedInAny $ map (numberify . toRangeTuples) input

part2 :: [String] -> String
part2 input = show $ length $ filter overlaps $ map (numberify . toRangeTuples) input

aoc :: IO ()
aoc = do
  readFile "aoc-data/day4.txt" >>= putStrLn . part1 . lines
  readFile "aoc-data/day4.txt" >>= putStrLn . part2 . lines

