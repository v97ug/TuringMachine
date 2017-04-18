module Main where

import Data.Maybe

data Direction = R | L | H

-- 第一リストは、注目しているリスト(ここでは、状態pの右側のテープ)
-- 第二リストは、パンくずリスト（ここでは、状態pの左側のテープ）
type ListZipper a = ([a], [a])

goRight :: ListZipper a -> ListZipper a
goRight (x:xs, bs) = (xs, x:bs)

goLeft :: ListZipper a -> ListZipper a
goLeft (xs, b:bs) = (b:xs, bs)

main :: IO ()
main = do
  let endB = replicate 3 "B"
--      endB = repeat "B"
      tapeChar = ["a", "b", "a"]
      (result, accum) = manipulateLoop (tapeChar ++ endB, endB) "p" []
  mapM_ (putStrLn . unwords) accum
  putStrLn "--Result--"
  putStrLn $ unwords result

manipulateLoop :: ListZipper String -> String -> [[String]] -> ([String], [[String]])
manipulateLoop (x:xs, crumbs) state accum = do
  let deltaRes = delta state x :: Maybe (String, String, Direction)

  if isNothing deltaRes then (reverse crumbs ++ (state : x : xs), accum)
  else
    let Just (newState, replace, direction) = deltaRes :: Maybe (String, String, Direction)
        fixedTape = (replace : xs, crumbs) :: ListZipper String
    in manipulateLoop (moveHead direction fixedTape) newState (accum ++ [reverse crumbs ++ (state : x : xs)])

moveHead :: Direction -> ListZipper String -> ListZipper String
moveHead H zipper = zipper
moveHead R zipper = goRight zipper
moveHead L zipper = goLeft zipper

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
