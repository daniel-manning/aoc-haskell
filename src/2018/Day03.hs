module Day03
    (
    ) where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, sepBy1, string)
import Data.Either.Combinators ( fromRight' )
import Data.Set (Set, fromList, intersection, empty, union)
import Data.List (find, delete)
import Data.Maybe (maybe)


newtype ID = ID Int deriving Show
data Position = Position Int Int deriving (Eq, Ord, Show)
data Dimensions = Dimensions Int Int deriving Show
data Claim = Claim ID Position Dimensions deriving Show

parseClaim :: Parser Claim
parseClaim = do
    char '#'
    id <- many1 digit
    string " @ "
    posX <- many1 digit
    char ','
    posY <- many1 digit
    string ": "
    dimX <- many1 digit
    char 'x'
    dimY <- many1 digit
    return $ Claim (ID (read id)) (Position (read posX) (read posY)) (Dimensions (read dimX) (read dimY))



readData :: IO [String]
readData = lines <$> readFile "resource/2018/day03"

readAndParse :: IO [Claim]
readAndParse = map (fromRight' .  parse parseClaim "") <$> readData
--------------------
{-- 
turn rectangles into sets of points and then fold the list
--}

turnClaimIntoPoints :: Claim -> (ID, Set Position)
turnClaimIntoPoints (Claim id (Position px py) (Dimensions dx dy)) = (id, fromList [Position x y | x <- [px .. (px + dx - 1)], y <- [py .. (py + dy - 1)]])

intersections :: Ord a => Set a -> [Set a] -> [Set a]
intersections x = map (`intersection` x)

foldUp :: Ord a => [Set a] -> Set a
foldUp [] = empty
foldUp (x : xs) = foldl union empty (intersections x xs) `union` foldUp xs

runPt1 :: [Claim] -> Int
runPt1 = length . foldUp . map (snd.turnClaimIntoPoints)
-----------------------

allOverlappingsByID::[(ID, Set Position)] -> [(ID, Set Position)]
allOverlappingsByID xs = map (\x -> (fst x, foldl union empty $ intersections (snd x) (delete (snd x) $ map snd xs))) xs

runPt2 :: [Claim] -> Int
runPt2 = (\(ID id) -> id) . maybe (ID 0) fst . find (null . snd) . allOverlappingsByID . map turnClaimIntoPoints

main :: IO Int
main =  runPt2 <$> readAndParse