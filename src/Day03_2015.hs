module Day03_2015
    (
    ) where

 import Data.List (nub, sort, group)

 data Position = Position Int Int deriving (Eq, Show)


 evaluateCommands:: Position -> [Char] -> [Position]
 evaluateCommands p [] = [p]
 evaluateCommands (Position x y) (c:cs) | c == '^' = [Position x y] ++ evaluateCommands (Position x     (y+1)) cs
                                        | c == '>' = [Position x y] ++ evaluateCommands (Position (x+1)  y)    cs
                                        | c == 'v' = [Position x y] ++ evaluateCommands (Position x     (y-1)) cs
                                        | c == '<' = [Position x y] ++ evaluateCommands (Position (x-1)  y)    cs
                                        | otherwise = []

 commandSanta = evaluateCommands (Position 0 0)

 howManyHouses :: String -> Int
 howManyHouses commands = length $ nub $ commandSanta commands

 howManyHousesPart2 :: String -> Int
 howManyHousesPart2 commands = length $ nub $ (commandSanta commandsHalf ++ commandSanta commandsOtherHalf)
   where
     commandsHalf = map fst $ filter (\l -> snd l `mod` 2 == 0) $ zip commands [1..]
     commandsOtherHalf = map fst $ filter (\l -> snd l `mod` 2 == 1) $ zip commands [1..]

 day03Part1 = howManyHouses <$> readString
 day03Part2 = howManyHousesPart2 <$> readString

 readString :: IO String
 readString = readFile "resource/2015/day03"