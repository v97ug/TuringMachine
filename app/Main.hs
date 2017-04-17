module Main where

import Lib

main :: IO ()
main = do
  let endB = replicate 3 "B"
      tape = endB ++ ["a", "b", "a"] ++ endB
  print tape

