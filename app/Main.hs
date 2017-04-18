module Main where

import Lib
import Data.Maybe
import Debug.Trace

data Direction = R | L | H

main :: IO ()
main = do
  let endB = replicate 3 "B"
      result = manipulateLoop endB "p" (["a", "b", "a"] ++ endB)
  putStrLn $ unwords result

manipulateLoop :: [String] -> String -> [String] -> [String]
manipulateLoop beforeP p afterAll@(target:afterP) = trace ((show . unwords) (beforeP ++ [p] ++ afterAll)) $ do
  let deltaRes = delta p target :: Maybe (String, String, Direction)

  if isNothing deltaRes then beforeP ++ (p : afterAll)
  else
    let Just (newP, replace, move) = deltaRes :: Maybe (String, String, Direction)
        (newBeforeP, newAfterP) = calBeforePAfterP move beforeP replace afterP
    in manipulateLoop newBeforeP newP newAfterP




calBeforePAfterP :: Direction -> [String] -> String -> [String] -> ([String], [String])
calBeforePAfterP H beforeP replace afterP = (beforeP, replace:afterP)
calBeforePAfterP R beforeP replace afterP = (beforeP ++ [replace], afterP)
calBeforePAfterP L beforeP replace afterP = (init beforeP, last beforeP : replace : afterP)

delta :: String -> String -> Maybe (String, String, Direction)
delta "p" "a" = Just ("p", "a", R)
delta "p" "b" = Just ("p", "b", R)
delta "p" "B" = Just ("q", "B", L)
delta "p" "a'" = Just ("q", "a", L)
delta "p" "b'" = Just ("q", "b", L)
delta "q" "a" = Just ("qa", "a'", L)
delta "q" "b" = Just ("qb", "b'", L)
delta "qa" "a" = Just ("qa", "a", L)
delta "qa" "b" = Just ("qa", "b'", L)
delta "qb" "a" = Just ("qb", "a", L)
delta "qb" "b" = Just ("qb", "b'", L)
delta "qa" "B" = Just ("ra", "B", L)
delta "qb" "B" = Just ("rb", "B", L)
delta "ra" "a" = Just ("ra", "a", L)
delta "ra" "b" = Just ("ra", "b", L)
delta "rb" "a" = Just ("rb", "a", L)
delta "rb" "b" = Just ("rb", "b", L)
delta "ra" "B" = Just ("s", "a", R)
delta "rb" "B" = Just ("s", "b", R)
delta "s" "a" = Just ("s", "a", R)
delta "s" "B" = Just ("p", "B", R)
delta "s" "b" = Just ("s", "b", R)
delta _ _ = Nothing
