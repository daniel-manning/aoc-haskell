module Day21_2021 where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, (<|>), string)
import Data.Either.Combinators ( fromRight' )


newtype TrackPlace = TrackPlace Int deriving Show
data Player = Player TrackPlace Int deriving Show

parseGame :: Parser TrackPlace
parseGame = do
    string "Player "
    n <- many1 digit
    string " starting position: "
    place <- many1 digit 
    return $ TrackPlace (read place)

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day21"

readAndParse :: IO (TrackPlace, TrackPlace)
readAndParse = (\xs -> (fromRight' $ parse parseGame "" (head xs), fromRight' $ parse parseGame "" (last xs)) ) <$> readData

determanistic3DieRoll :: Int -> Int
determanistic3DieRoll n = 9*n -3

runTurn :: Player -> Int -> Player
runTurn (Player (TrackPlace p) scores) n = Player (TrackPlace np') (np' + scores)
    where
        np = p + determanistic3DieRoll n
        np' = if np `mod` 10 == 0 then 10 else np `mod` 10

--runGame :: (Player, Player, Int) -> Int
runGame (p1, p2, turn) | checkScore p1' >= 1000  = 3*turn*checkScore p2
                       | checkScore p2' >= 1000  = 3*(turn+1)*checkScore p1'
                       | otherwise = runGame (p1', p2', turn + 2)
    where
        checkScore (Player _ score) = score
        p1' = runTurn p1 turn
        p2' = runTurn p2 (turn+1)

runPt1 :: (TrackPlace, TrackPlace) -> Int
runPt1 (p1, p2) = runGame (Player p1 0, Player p2 0, 1)

solution :: IO Int
solution = runPt1 <$> readAndParse
