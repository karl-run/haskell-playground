module Aoc.Day8 (aoc) where

import Data.Char
import Data.List (elemIndex, transpose)

type Matrix = [[Int]]

type Pos = (Int, Int)

getItem :: Matrix -> Pos -> Int
getItem matrix (x, y) = (matrix !! y) !! x

getLeft :: Matrix -> Pos -> [Int]
getLeft matrix (x, y) = reverse $ take x row
  where
    row = matrix !! y

getRight :: Matrix -> Pos -> [Int]
getRight matrix (x, y) = drop (x + 1) row
  where
    row = matrix !! y

getUp :: Matrix -> Pos -> [Int]
getUp matrix (x, y) = reverse $ take y row
  where
    row = transpose matrix !! x

getDown :: Matrix -> Pos -> [Int]
getDown matrix (x, y) = drop (y + 1) row
  where
    row = transpose matrix !! x

getDirections :: Matrix -> Pos -> ([Int], [Int], [Int], [Int])
getDirections matrix pos = (getLeft matrix pos, getRight matrix pos, getUp matrix pos, getDown matrix pos)

toNumberMatrix :: [String] -> Matrix
toNumberMatrix = map (map digitToInt)

isBlocked :: Int -> [Int] -> Bool
isBlocked value xs = maximum xs >= value

isBlockedAllDirections :: Matrix -> Pos -> Bool
isBlockedAllDirections matrix pos =
  isBlocked value left
    && isBlocked value right
    && isBlocked value up
    && isBlocked value down
  where
    value = getItem matrix pos
    (left, right, up, down) = getDirections matrix pos

countVisibleInside :: Matrix -> Int
countVisibleInside matrix = length $ filter not $ map isTreeBlockedInMatrix treesToCheck
  where
    isTreeBlockedInMatrix = isBlockedAllDirections matrix
    treesToCheck = [(x, y) | x <- [1 .. length (head matrix) - 2], y <- [1 .. length matrix - 2]]

nearestBlockingValue :: Int -> [Int] -> Int
nearestBlockingValue _ [] = -1
nearestBlockingValue value (x : xs) = if x >= value then x else nearestBlockingValue value xs

distanceToBigger :: Int -> [Int] -> Int
distanceToBigger value xs =
  case blockingValue `elemIndex` xs of
    Just index -> index + 1
    Nothing -> length xs
  where
    blockingValue = value `nearestBlockingValue` xs

calculateViewingDistance :: Matrix -> Pos -> Int
calculateViewingDistance matrix pos =
  distanceToBigger value up
    * distanceToBigger value left
    * distanceToBigger value down
    * distanceToBigger value right
  where
    value = getItem matrix pos
    (left, right, up, down) = getDirections matrix pos

calculateBestTreeScore :: Matrix -> Int
calculateBestTreeScore matrix = maximum [calculateViewingDistanceInMatrix tree | tree <- treesToCheck]
  where
    calculateViewingDistanceInMatrix = calculateViewingDistance matrix
    treesToCheck = [(x, y) | x <- [1 .. length (head matrix) - 2], y <- [1 .. length matrix - 2]]

part1 :: [String] -> String
part1 input = show $ visibleInside + visibleOutside
  where
    matrix = toNumberMatrix input
    visibleInside = countVisibleInside matrix
    visibleOutside = ((length (head matrix) + length matrix) * 2) - 4

part2 :: [String] -> String
part2 input = show bestScore
  where
    matrix = toNumberMatrix input
    bestScore = calculateBestTreeScore matrix

aoc :: IO ()
aoc = do
  -- 1809
  readFile "aoc-data/day8.txt" >>= putStrLn . part1 . lines
  -- 479400
  readFile "aoc-data/day8.txt" >>= putStrLn . part2 . lines
