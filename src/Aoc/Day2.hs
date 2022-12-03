module Aoc.Day2 (aoc) where

toTuple :: [String] -> (String, String)
toTuple [x, y] = (x, y)

splitPairString :: String -> (String, String)
splitPairString string = toTuple $ words string

fixPlayerValue :: [(String, String)] -> [(String, String)]
fixPlayerValue values =
  [ ( fst tuple,
      case snd tuple of
        "X" -> "A"
        "Y" -> "B"
        "Z" -> "C"
    )
    | tuple <- values
  ]

splitToTuples :: [String] -> [(String, String)]
splitToTuples = map splitPairString

-- X = lose
-- Y = draw
-- Z = win
scoreTuplePairForWin :: (String, String) -> Int
scoreTuplePairForWin ("A", "X") = 0 + 3
scoreTuplePairForWin ("A", "Y") = 3 + 1
scoreTuplePairForWin ("A", "Z") = 6 + 2
scoreTuplePairForWin ("B", "X") = 0 + 1
scoreTuplePairForWin ("B", "Y") = 3 + 2
scoreTuplePairForWin ("B", "Z") = 6 + 3
scoreTuplePairForWin ("C", "X") = 0 + 2
scoreTuplePairForWin ("C", "Y") = 3 + 3
scoreTuplePairForWin ("C", "Z") = 6 + 1
scoreTuplePairForWin _ = 1000

scoreForWin :: (String, String) -> Int
scoreForWin tuple
  | player > npc = 6
  | npc > player = 0
  | npc == player = 3
  where
    npc = fst tuple
    player = snd tuple

scoreForHand :: (String, String) -> Int
scoreForHand tuple
  | playerHand == "A" = 1
  | playerHand == "B" = 2
  | playerHand == "C" = 3
  where
    playerHand = snd tuple

--score :: [(String, String)] -> [Int]
score tuples = [(fst x, snd x, scoreForWin x + scoreForHand x) | x <- tuples]

solve :: [String] -> String
solve input = show $ sum $ map scoreTuplePairForWin $ splitToTuples input

aoc :: IO ()
aoc = do
  readFile "aoc-data/day2.txt" >>= putStrLn . solve . lines
