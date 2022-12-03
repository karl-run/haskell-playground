module Book.Chapter4 (chapter4) where

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

initials :: String -> String -> String
initials (f : _) (l : _) = [f] ++ ". " ++ [l] ++ "."

chapter4 :: IO ()
chapter4 = do
  print "firstColour"

  print (tell ([1, 2] :: [Int]))
  print (initials "Karl" "Overå")
