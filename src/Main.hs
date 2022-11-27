module Main (main) where

import Chapter2
import Chapter3

main :: IO ()
main = do
  print "Chapter 2:"
  Chapter2.chapter2
  print "Chapter 3:"
  Chapter3.chapter3
