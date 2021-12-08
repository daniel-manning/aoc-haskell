module Day01 where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, sepBy1, string, (<|>))
import Data.Either.Combinators ( fromRight' )
import Data.Set (Set, fromList, intersection, empty, union, elemAt)
import Data.List (find, delete)
import Data.Maybe (maybe, fromJust)

data Command = R Int | L Int deriving Show
data Compass = N | E | S | W deriving Show
data Position = Position Int Int deriving (Eq, Ord, Show)
data Walker = Walker Position Compass deriving Show

-------------------
parseRight = do
    char 'R'
    n <- many1 digit
    return $ R (read n)

parseLeft = do
    char 'L'
    n <- many1 digit
    return $ L (read n)

parseCommand :: Parser Command
parseCommand = parseRight <|> parseLeft

parseList :: Parser [Command]
parseList = sepBy1 parseCommand (string ", ")

readData :: IO String
readData = readFile "resource/2016/day01"

readAndParse :: IO [Command]
readAndParse = fromRight' . parse parseList "" <$> readData
---------------------
runCommand :: Walker -> Command -> Walker
runCommand (Walker (Position x y) N) (R d) = Walker (Position (x + d) y) E
runCommand (Walker (Position x y) N) (L d) = Walker (Position (x - d) y) W
runCommand (Walker (Position x y) E) (R d) = Walker (Position x (y + d)) S
runCommand (Walker (Position x y) E) (L d) = Walker (Position x (y - d)) N
runCommand (Walker (Position x y) S) (R d) = Walker (Position (x - d) y) W
runCommand (Walker (Position x y) S) (L d) = Walker (Position (x + d) y) E
runCommand (Walker (Position x y) W) (R d) = Walker (Position x (y - d)) N
runCommand (Walker (Position x y) W) (L d) = Walker (Position x (y + d)) S



distance :: Position -> Int
distance (Position x y) = abs x + abs y

distanceAway :: Walker -> Int
distanceAway (Walker p _) = distance p

start = Walker (Position 0 0) N

runPt1 :: [Command] -> Int
runPt1 = distanceAway . foldl runCommand start
--------------------

checkFirstRevisit :: Set Position -> Walker -> [Command] -> Maybe (Set Position)
checkFirstRevisit _ _ [] = Nothing
checkFirstRevisit p w@(Walker cp compass) (c:cs) | not $ null (d `intersection` p) = Just (d `intersection` p)
                                                 | otherwise = checkFirstRevisit (d `union` p) k cs
    where
        k = runCommand w c
        dist = case c of
            R l -> l
            L l -> l
        comp = (\(Walker _ comp')-> comp') k
        px = (\(Walker (Position x _) _)-> x) k
        py = (\(Walker (Position _ y) _)-> y) k
        d = case comp of
            N -> fromList [Position px y | y <- [py .. (py + dist - 1)]]
            E -> fromList [Position x py | x <- [(px - dist + 1) .. px]]
            S -> fromList [Position px y | y <- [(py - dist + 1) .. py]]
            W -> fromList [Position x py | x <- [px .. (px + dist - 1)]]

runPt2 :: [Command] -> Int
runPt2 = distance . elemAt 0 . fromJust . checkFirstRevisit empty start

solution :: IO Int
solution = runPt2 <$> readAndParse