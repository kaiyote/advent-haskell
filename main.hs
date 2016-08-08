module Main where

import Data.Text (strip, unpack, pack)
import Data.List (elemIndex, sort)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  day1Input <- readFile "./input/day1.txt"
  putStrLn $ "Day 1 Part 1: " ++ show (day1Part1 day1Input)
  putStrLn $ "Day 1 Part 2: " ++ show (day1Part2 day1Input)
  day2Input <- readFile "./input/day2.txt"
  putStrLn $ "Day 2 Part 1: " ++ show (day2Part1 day2Input)
  putStrLn $ "Day 2 Part 2: " ++ show (day2Part2 day2Input)

trim :: String -> String
trim = unpack . strip . pack

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn item list = front : splitOn item (drop (1 + length front) list)
  where
    front = takeWhile (/= item) list

stringToNumberList :: Char -> String -> [Int]
stringToNumberList splitChar = map (\x -> read x :: Int) . splitOn splitChar

day1Part1 :: String -> Int
day1Part1 = foldl (\acc c -> if c == ')' then acc - 1 else acc + 1) 0 . trim

day1Part2 :: String -> Int
day1Part2 = fromMaybe 0 . elemIndex (negate 1) . scanl (\acc c -> if c == ')' then acc - 1 else acc + 1) (0 :: Int) . trim

day2Part1 :: String -> Int
day2Part1 = sum . map ((\[l, w, h] -> 2 * l * w + 2 * w * h + 2 * l * h + l * w) . sort . stringToNumberList 'x') . lines . trim

day2Part2 :: String -> Int
day2Part2 = sum . map ((\[l, w, h] -> l * w * h + 2 * (l + w)) . sort . stringToNumberList 'x') . lines . trim
