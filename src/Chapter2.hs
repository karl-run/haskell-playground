module Chapter2 (chapter2) where

-- Exercises for Chapter 2:

-- Find the penultimate element in list l
-- penultimate l = last (init l)

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
findK :: Int -> [Int] -> Int
findK k l = l !! k

-- Determine if list l is a palindrome
isPalindrome :: [Int] -> Bool
isPalindrome l = l == reverse l

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list.
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
duplicate :: [Int] -> [Int]
duplicate xs = concat [[x, x] | x <- xs]

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}
shortestArrayLength :: [Int] -> [Char] -> Int
shortestArrayLength a1 a2 = min (length a1) (length a2) - 1

ziplike :: [Int] -> [Char] -> [(Int, Char)]
ziplike xs ys = [(xs !! index, ys !! index) | index <- [0 .. shortestArrayLength xs ys]]

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
splitAtIndex :: Int -> [Int] -> ([Int], [Int])
splitAtIndex k l = (take k l, drop k l)

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK :: Int -> [Int] -> [Int]
dropK k l = take k l ++ drop (k + 1) l

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice :: Int -> Int -> [Int] -> [Int]
slice i k l = drop i (take k l)

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem :: Int -> Int -> [Int] -> [Int]
insertElem x k l = take k l ++ [x] ++ drop k l

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate :: Int -> [Int] -> [Int]
rotate n l = drop n l ++ take n l

chapter2 :: IO ()
chapter2 = do
  print "findK"
  print (findK 2 [0, 0, 1, 0, 0, 0])
  print "isPalindrome"
  print (isPalindrome [0, 1, 2, 1, 0], isPalindrome [0, 1, 2, 2, 0])
  print "duplicate"
  print (duplicate [1, 2, 3])
  print "ziplike"
  print (ziplike [1, 2, 3] ['a', 'b', 'c', 'd'])
  print "splitAtIndex"
  print (splitAtIndex 3 [1, 1, 1, 2, 2, 2])
  print "dropK"
  print (dropK 3 [0, 0, 0, 1, 0, 0, 0])
  print "slice"
  print (slice 3 6 [0, 0, 0, 1, 2, 3, 0, 0, 0])
  print "insertElem"
  print (insertElem 2 5 [0, 0, 0, 0, 0, 0])
  print "rotate"
  print (rotate 2 [1, 2, 3, 4, 5])
