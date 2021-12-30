{-# LANGUAGE TupleSections #-}
module Day20_2021 where

import Text.ParserCombinators.Parsec (many1, letter, Parser, parse, char, string, (<|>), digit)
import Data.Either.Combinators ( fromRight' )
import Data.String.Utils (rstrip)
import Data.Function (on)
import Data.List (sort, nub, maximumBy, find)
import Data.Maybe (fromMaybe)
import ListUtils(Position, Position (..), convertToPositionList)
import Data.Set (Set, fromList, toList, empty, union)
import Data.List.Extra (notNull)

data Pixel = Light | Dark deriving (Eq, Show)

type Floor = [(Position, Pixel)]
type IEA = [Pixel]

parsePixels:: Parser [Pixel]
parsePixels = do
    x1 <- many1 (char '.' <|> char '#')
    return $ map (\c -> if c == '.' then Dark else Light) x1


parseIEAFloor :: [String] -> (IEA, Floor)
parseIEAFloor xs = (fromRight' $ parse parsePixels "" (head xs), onlyLight $ convertToPositionList $ map (fromRight' . parse parsePixels "") (takeWhile notNull $ drop 2 xs))

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day20"
-------------------

readAndParse :: IO (IEA, Floor)
readAndParse = parseIEAFloor <$> readData


lookupIEAPixel :: IEA -> Int -> Pixel
lookupIEAPixel iea n = iea !! n

getNeighbourhood :: Position -> [Position]
getNeighbourhood (Position x y) = [Position (x-1) (y-1), Position x (y - 1), Position (x+1) (y - 1),
                                   Position (x - 1) y,   Position x y,       Position (x + 1) y,
                                   Position (x-1) (y+1), Position x (y + 1), Position (x+1) (y+1)]

lookupPL :: [(Position, a)] -> Position -> a -> a
lookupPL pas p d = snd $ fromMaybe (p,d) $ find (\l -> fst l == p) pas

reduceBinary :: [Int] -> Int
reduceBinary bs = foldl (\ b a -> b + fst a*2^snd a) 0 $ reverse bs `zip` [0..]

neighbourhoodToValue ::  Floor -> [Position] -> Int
neighbourhoodToValue floor pos = reduceBinary $ map ((\pixel -> if pixel == Dark then 0 else 1) . (\p -> lookupPL floor p Dark)) pos 


evolvePixel :: Position -> IEA -> Floor -> (Position, Pixel)
evolvePixel p iea floor = (p,) $ lookupIEAPixel iea $ neighbourhoodToValue floor $ getNeighbourhood p

onlyLight :: Floor -> Floor
onlyLight = filter (\l -> snd l == Light)

howManyLit :: Floor -> Int 
howManyLit = length . onlyLight

-- IEA is bi-stable for completely empty neighbourhoods
-- so instead of a set we need to completely fill a new boundary around the grid for each step
-- for the bistable examples we need to alternate between a boundary of 1 and a boundary of 0 to account for flashing!
everyNeighbourhoodThatContainsPixel :: Position -> Set Position
everyNeighbourhoodThatContainsPixel (Position x y) = fromList [Position (x+a) (y+b) | a <- [(-2)..2], b <- [(-2)..2]]

enhance :: IEA -> Floor -> Floor
enhance iea floor = onlyLight $ map (\p -> evolvePixel p iea floor) $ toList $ foldl (\b a -> b `union` everyNeighbourhoodThatContainsPixel a) empty $ map fst floor

solution = (\l -> howManyLit $ enhance (fst l) $ enhance (fst l) (snd l)) <$> readAndParse