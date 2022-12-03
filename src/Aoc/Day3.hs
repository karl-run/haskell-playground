module Aoc.Day3 (aoc) where

import Data.Char (ord)

splitAtMiddle :: String -> (String, String)
splitAtMiddle value = splitAt middle value
  where
    middle = length value `div` 2

filterEqualChar :: [(Char, Char)] -> [(Char, Char)]
filterEqualChar = filter isTupleEqual
  where
    isTupleEqual :: (Char, Char) -> Bool
    isTupleEqual (a, b) = a == b

filterEqualCharTriple :: [(Char, Char, Char)] -> [(Char, Char, Char)]
filterEqualCharTriple = filter isTripleEqual
  where
    isTripleEqual :: (Char, Char, Char) -> Bool
    isTripleEqual (a, b, z) = a == b && b == z

deduplicate :: (String, String) -> Char
deduplicate (first, second) = fst $ head $ filterEqualChar tuplePairs
  where
    tuplePairs = [(x, y) | x <- first, y <- second]

detriplicate :: (String, String, String) -> Char
detriplicate (first, second, third) = fstTriple $ head $ filterEqualCharTriple triples
  where
    triples = [(x, y, z) | x <- first, y <- second, z <- third]
    fstTriple (a, _, _) = a

score :: Char -> Int
score char
  | value >= 96 && value <= 122 = value - 96
  | value >= 65 && value <= 90 = value - 38
  | otherwise = error ("Invalid character: " ++ [char])
  where
    value = ord char

chunk :: Int -> [String] -> [[String]]
chunk _ [] = []
chunk n xs = [take n xs] ++ chunk n (drop n xs)

triple :: [String] -> (String, String, String)
triple [a, b, c] = (a, b, c)
triple _ = error "Not a triple"

part1 :: [String] -> String
part1 input = "Part 1: " ++ show (sum $ map (score . deduplicate . splitAtMiddle) input)

part2 :: [String] -> String
part2 input = "Part 2: " ++ show (sum $ map (score . detriplicate . triple) $ chunk 3 input)

aoc :: IO ()
aoc = do
  readFile "aoc-data/day3.txt" >>= putStrLn . part1 . lines
  readFile "aoc-data/day3.txt" >>= putStrLn . part2 . lines
