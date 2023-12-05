module Day2.Part2 (run) where

import           Data.List.Split (splitOn)
import           Data.Maybe      (fromMaybe)
import           Text.Read       (readMaybe)

data Game = Game GameId [Roll] deriving (Show)

type GameId = Int

data Roll = Roll { red :: Maybe Int, green :: Maybe Int, blue :: Maybe Int } deriving (Show)

inputPath :: String
inputPath = "src/Day2/input-full.txt"

run :: IO ()
run = do
  print "Day 2 - Part 2"
  input <- readInputFile
  let games = map parseGame input
  let minDieCounts = map findMinDieCounts games
  print $ sum $ map rollPower minDieCounts
  return ()

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile inputPath
  return (lines contents)

maxRoll :: Roll
maxRoll = Roll { red = Just 12, green = Just 13, blue = Just 14 }

findMinDieCounts :: Game -> Roll
findMinDieCounts (Game _ rolls) = foldl findMinDie' (Roll Nothing Nothing Nothing) rolls
  where findMinDie' :: Roll -> Roll -> Roll
        findMinDie' minRoll roll = Roll { red = max (red minRoll) (red roll), green = max (green minRoll) (green roll), blue = max (blue minRoll) (blue roll) }

rollPower :: Roll -> Int
rollPower (Roll r g b) = product $ map (fromMaybe 1) [r, g, b]

parseGame :: String -> Game
parseGame input | length splitInput == 2 = Game (read $ last $ words $ head splitInput) (map parseRoll $ splitOn ";" $ last splitInput)
                | otherwise = error "Invalid input"
  where splitInput = splitOn ":" input

parseRoll :: String -> Roll
parseRoll input = foldl parseRoll' (Roll Nothing Nothing Nothing) splitInput
  where splitInput = splitOn "," input
        parseRoll' :: Roll -> String -> Roll
        parseRoll' (Roll r g b) die = case words die of
          [count, "red"]   -> Roll (readMaybe count) g b
          [count, "green"] -> Roll r (readMaybe count) b
          [count, "blue"]  -> Roll r g (readMaybe count)
          _                -> Roll r g b

