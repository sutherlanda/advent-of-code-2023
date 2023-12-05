module Day1.Part2 (run) where

import           Control.Monad  (join)
import           Data.Bifunctor (Bifunctor (bimap))
import           Data.Char      (digitToInt, intToDigit, isDigit)
import           Data.List      (isPrefixOf)

run :: IO ()
run = do
  putStrLn "Day 1 - Part 2"
  inputLines <- readInputFile
  let firstDigits = map findFirstDigit inputLines
  let lastDigits = map findLastDigit inputLines
  let pairs = zip firstDigits lastDigits
  print (sum (map (read . joiner . join bimap intToDigit) pairs :: [Int]))
    where
      joiner :: (Char, Char) -> [Char]
      joiner (a, b) = [a,b]

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "src/Day1/input-full.txt"
  return (lines contents)

findFirstDigit :: String -> Int
findFirstDigit [] = error "No digit found"
findFirstDigit str | "one" `isPrefixOf` str = 1
                   | "two" `isPrefixOf` str = 2
                   | "three" `isPrefixOf` str = 3
                   | "four" `isPrefixOf` str = 4
                   | "five" `isPrefixOf` str = 5
                   | "six" `isPrefixOf` str = 6
                   | "seven" `isPrefixOf` str = 7
                   | "eight" `isPrefixOf` str = 8
                   | "nine" `isPrefixOf` str = 9
findFirstDigit (x:xs) | isDigit x = digitToInt x
                      | otherwise = findFirstDigit xs

findLastDigit :: String -> Int
findLastDigit str = findLastDigit' $ reverse str
  where
    findLastDigit' [] = error "No digit found"
    findLastDigit' str' | "eno" `isPrefixOf` str' = 1
                      | "owt" `isPrefixOf` str' = 2
                      | "eerht" `isPrefixOf` str' = 3
                      | "ruof" `isPrefixOf` str' = 4
                      | "evif" `isPrefixOf` str' = 5
                      | "xis" `isPrefixOf` str' = 6
                      | "neves" `isPrefixOf` str' = 7
                      | "thgie" `isPrefixOf` str' = 8
                      | "enin" `isPrefixOf` str' = 9
    findLastDigit' (x:xs) | isDigit x = digitToInt x
                          | otherwise = findLastDigit' xs
