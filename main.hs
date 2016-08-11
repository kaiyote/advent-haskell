module Main where

import Data.Text (strip, unpack, pack)
import Data.List (elemIndex, sort, partition, find, isPrefixOf)
import Data.Set (fromList, size)
import Data.Maybe (fromMaybe)
import Data.Array (Array, assocs, listArray)
import Control.Arrow ((&&&))
import Text.Regex.PCRE ((=~))
import qualified Data.Hash.MD5 as M

main :: IO ()
main = do
  day1Input <- readFile "./input/day1.txt"
  putStrLn $ "Day 1 Part 1: " ++ show (day1Part1 day1Input)
  putStrLn $ "Day 1 Part 2: " ++ show (day1Part2 day1Input)
  day2Input <- readFile "./input/day2.txt"
  putStrLn $ "Day 2 Part 1: " ++ show (day2Part1 day2Input)
  putStrLn $ "Day 2 Part 2: " ++ show (day2Part2 day2Input)
  day3Input <- readFile "./input/day3.txt"
  putStrLn $ "Day 3 Part 1: " ++ show (day3Part1 day3Input)
  putStrLn $ "Day 3 Part 2: " ++ show (day3Part2 day3Input)
  {-putStrLn $ "Day 4 Part 1: " ++ show (day4Part1 day4Input)
  putStrLn $ "Day 4 Part 2: " ++ show (day4Part2 day4Input)-}
  day5Input <- readFile "./input/day5.txt"
  putStrLn $ "Day 5 Part 1: " ++ show (day5Part1 day5Input)
  putStrLn $ "Day 5 Part 2: " ++ show (day5Part2 day5Input)
  {-where
    day4Input = "iwrupvqb"-}

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

day3Part1 :: String -> Int
day3Part1 = size . fromList . scanl move (0, 0) . trim
  where
    move :: (Int, Int) -> Char -> (Int, Int)
    move (x, y) c = case c of
      '^' -> (x, y - 1)
      'v' -> (x, y + 1)
      '<' -> (x - 1, y)
      '>' -> (x + 1, y)
      _ -> (x, y)

day3Part2 :: String -> Int
day3Part2 = size . fromList . concatMap (scanl move (0, 0) . map snd) . tupToList . partition everyOther . assocs . indexedInput . trim
  where
    move :: (Int, Int) -> Char -> (Int, Int)
    move (x, y) c = case c of
      '^' -> (x, y - 1)
      'v' -> (x, y + 1)
      '<' -> (x - 1, y)
      '>' -> (x + 1, y)
      _ -> (x, y)

    indexedInput :: String -> Array Int Char
    indexedInput str = listArray (0, length str - 1) str

    tupToList :: ([a], [a]) -> [[a]]
    tupToList tup = [fst tup, snd tup]

    everyOther :: (Int, a) -> Bool
    everyOther (i, _) = i `mod` 2 == 0

day4Part1 :: String -> Int
day4Part1 input = snd . fromMaybe ("", 0) . find (isPrefixOf "00000" . fst) $ map (M.md5s . M.Str . (++) input . show &&& id) ([1..] :: [Int])

day4Part2 :: String -> Int
day4Part2 input = snd . fromMaybe ("", 0) . find (isPrefixOf "000000" . fst) $ map (M.md5s . M.Str . (++) input . show &&& id) ([1..] :: [Int])

day5Part1 :: String -> Int
day5Part1 = length . filter isGood . lines . trim
  where
    isGood :: String -> Bool
    isGood line = all ($ line) [threeVowels, doubleLetter, noBadString]

    threeVowels :: String -> Bool
    threeVowels = flip (=~) "[aeiou].*[aeiou].*[aeiou]"

    doubleLetter :: String -> Bool
    doubleLetter = flip (=~) "([a-z])\\1"

    noBadString :: String -> Bool
    noBadString = not . flip (=~) "(ab|cd|pq|xy)"

day5Part2 :: String -> Int
day5Part2 = length . filter isGood . lines . trim
  where
    isGood :: String -> Bool
    isGood line = all ($ line) [doublePair, eyePair]

    doublePair :: String -> Bool
    doublePair = flip (=~) "([a-z])([a-z]).*\\1\\2"

    eyePair :: String -> Bool
    eyePair = flip (=~) "([a-z])[a-z]\\1"
