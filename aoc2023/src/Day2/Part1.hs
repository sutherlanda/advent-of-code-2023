module Day2.Part1 (run) where

import           Data.List.Split (splitOn)
import           Text.Read       (readMaybe)

data Game = Game GameId [Roll] deriving (Show)

type GameId = Int

data Roll = Roll { red :: Maybe Int, green :: Maybe Int, blue :: Maybe Int } deriving (Show)

inputPath :: String
inputPath = "src/Day2/input-full.txt"

run :: IO ()
run = do
  print "Day 2 - Part 1"
  input <- readInputFile
  let games = map parseGame input
  let possibleGames = filter isGamePossible games
  let possibleGameIds = map (\(Game gameId _) -> gameId) possibleGames
  print $ sum possibleGameIds

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile inputPath
  return (lines contents)

isRollPossible :: Roll -> Bool
isRollPossible (Roll r g b) = r <= red maxRoll && g <= green maxRoll && b <= blue maxRoll

isGamePossible :: Game -> Bool
isGamePossible (Game _ rolls) = all isRollPossible rolls

maxRoll :: Roll
maxRoll = Roll { red = Just 12, green = Just 13, blue = Just 14 }

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

