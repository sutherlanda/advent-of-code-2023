module Day1.Part1 (run) where

import           Control.Monad  (join)
import           Data.Bifunctor (Bifunctor (bimap))
import           Data.Char      (digitToInt, intToDigit, isDigit)

run :: IO ()
run = do
  putStrLn "Day 1 - Part 1"
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
findFirstDigit (x:xs) | isDigit x = digitToInt x
                      | otherwise = findFirstDigit xs

findLastDigit :: String -> Int
findLastDigit input = reverse input |> findFirstDigit

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

