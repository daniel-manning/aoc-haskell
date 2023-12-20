{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Day10_2023
    (
    ) where

import Data.List (find, foldl, length, groupBy, sortBy, sort)
import Data.Maybe (mapMaybe, fromJust, isJust)
import Data.Map (Map)
import qualified Data.Map.Lazy as Map (lookup, fromList)
import Grid (neighbourhoodOnInfGridWithoutD)
import ListUtils (Position(..), convertToPositionList, createPositionGrid)
import Models ( Position(..) )
import Data.Tuple.Extra (thd3)
import Data.Either.Validation (_Failure)

data Tile = VPipe | HPipe | NEBend | NWBend | SWBend | SEBend | Ground | Animal deriving Eq
type TileField = [[Tile]]

instance Show Tile where
    show VPipe  = "|"
    show HPipe   = "-"
    show NEBend   = "L"
    show NWBend   = "J"
    show SWBend  = "7" 
    show SEBend  = "F" 
    show Ground  = "."
    show Animal  = "S"

printGrid :: Show a => [[a]] -> IO ()
printGrid = mapM_ (putStrLn . textRepresentation)

textRepresentation :: Show a => [a] -> String
textRepresentation = foldl (\a y -> a ++ show y) ""

toTile :: Char -> Maybe Tile
toTile '|' = Just VPipe
toTile '-' = Just HPipe 
toTile 'L' = Just NEBend 
toTile 'J' = Just NWBend 
toTile '7' = Just SWBend 
toTile 'F' = Just SEBend 
toTile '.' = Just Ground
toTile 'S' = Just Animal
toTile _ = Nothing

readData :: IO [[Tile]]
readData = map (mapMaybe toTile) . lines <$> readFile "resource/2023/day10"
----------------------------

data Compass = North | East | South | West deriving (Eq, Ord, Show)

moveAlongPipe :: Compass -> Tile -> Position -> Maybe (Position, Compass)
moveAlongPipe North VPipe (Position x y) = Just (Position x (y-1), North)
moveAlongPipe South VPipe (Position x y) = Just (Position x (y+1), South)
moveAlongPipe East VPipe _ = Nothing
moveAlongPipe West VPipe _ = Nothing

moveAlongPipe East HPipe (Position x y) = Just (Position (x+1) y, East)
moveAlongPipe West HPipe (Position x y) = Just (Position (x-1) y, West)
moveAlongPipe North HPipe _ = Nothing
moveAlongPipe South HPipe _ = Nothing

moveAlongPipe South NEBend (Position x y) = Just (Position (x+1) y, East)
moveAlongPipe West NEBend (Position x y) = Just (Position x (y-1), North)
moveAlongPipe East NEBend _ = Nothing
moveAlongPipe North NEBend _ = Nothing

moveAlongPipe South NWBend (Position x y) = Just (Position (x-1) y, West)
moveAlongPipe East NWBend (Position x y) = Just (Position x (y-1), North)
moveAlongPipe West NWBend _ = Nothing
moveAlongPipe North NWBend _ = Nothing

moveAlongPipe East SWBend (Position x y) = Just (Position x (y+1), South)
moveAlongPipe North SWBend (Position x y) = Just (Position (x-1) y, West)
moveAlongPipe West SWBend _ = Nothing
moveAlongPipe South SWBend _ = Nothing

moveAlongPipe North SEBend (Position x y) = Just (Position (x+1) y, East)
moveAlongPipe West SEBend (Position x y) = Just (Position x (y+1), South)
moveAlongPipe South SEBend _ = Nothing
moveAlongPipe East SEBend _ = Nothing

loopWalk :: Compass -> Position -> Position -> Map Position Tile -> [Position]
loopWalk c p fp mpt | p == fp = [p]
                    | otherwise = p : loopWalk c' p' fp mpt
    where
        t = fromJust $ Map.lookup p mpt
        (p', c') = fromJust $ moveAlongPipe c t p

startingPosition :: [(Position, Tile)] -> Position
startingPosition = fst . fromJust . find (\(_, t) -> t == Animal)

isConnected :: Position -> (Position, Tile) -> Bool
isConnected (Position x y) (Position x1 y1, t) | x > x1 && y == y1 = t == HPipe || t == NEBend || t ==  SEBend
                                               | x < x1 && y == y1 = t == HPipe || t == NWBend || t ==  SWBend
                                               | x == x1 && y > y1 = t == VPipe || t == SEBend || t ==  SWBend
                                               | x == x1 && y < y1 = t == VPipe || t == NEBend || t ==  NWBend

compass :: Position -> (Position, Tile) -> Compass
compass (Position x y) (Position x1 y1, t) | x > x1 && y == y1 = West
                                           | x < x1 && y == y1 = East
                                           | x == x1 && y > y1 = North
                                           | x == x1 && y < y1 = South

directionOfFirstStep :: [(Position, Tile)] -> Position -> [(Compass, Position)]
directionOfFirstStep ps start = map (\l -> (compass start l, fst l)) ns
    where
        neighbourhood = neighbourhoodOnInfGridWithoutD start
        neighbourhood' = filter (\(p, _) -> p `elem` neighbourhood) ps
        ns = filter (isConnected start) neighbourhood'

findPathFromStart :: [(Position, Tile)] -> [Position]
findPathFromStart ps = loopWalk c p start (Map.fromList ps)
    where
        start = startingPosition ps
        directions = directionOfFirstStep ps start
        (c, p) = head directions

furthestAway :: [Position] -> Int
furthestAway ps = length ps `div` 2

runPt1 =  furthestAway . findPathFromStart . convertToPositionList <$> readData
--------------------------
newtype PathTiles = PathTiles (Map Position Tile) -- Restricted to just the positions of the path

rayCastLine :: Foldable t => PathTiles -> Tile -> t Position -> [Position] --reduced list of just the INSIDE elements
rayCastLine pts replacement = thd3 . foldl (trackBorderCrossing pts replacement)  (0, Nothing, [])

isBend :: Maybe Tile -> Bool
isBend t = t == Just SWBend || t == Just SEBend || t == Just NEBend || t == Just NWBend

samePolarity :: Maybe Tile -> Maybe Tile -> Bool
samePolarity a b | a == b = True
samePolarity (Just SWBend) (Just SEBend) = True
samePolarity (Just SEBend) (Just SWBend) = True
samePolarity (Just NEBend) (Just NWBend) = True
samePolarity (Just NWBend) (Just NEBend) = True
samePolarity _ _ = False

trackBorderCrossing :: PathTiles -> Tile -> (Int, Maybe Tile, [Position]) -> Position -> (Int, Maybe Tile, [Position])
trackBorderCrossing (PathTiles mpt) replacement (n, mt, ps) p = trackBorderCrossing' (PathTiles mpt) replacement (n, mt, ps) p (Map.lookup p mpt)

trackBorderCrossing' (PathTiles mpt) replacement (n, mt, ps) p (Just VPipe) = (n + 1, mt, ps)
trackBorderCrossing' (PathTiles mpt) replacement (n, mt, ps) p (Just Animal) = trackBorderCrossing' (PathTiles mpt) replacement (n, mt, ps) p (Just replacement)
trackBorderCrossing' (PathTiles mpt) replacement (n, mt, ps) p t | isJust mt && isBend t && not (samePolarity mt t) = (n, Nothing, ps)
                                                  | isJust mt && isBend t && samePolarity mt t = (n + 1, Nothing, ps)
                                                  | isBend t = (n + 1, t, ps)
                                                  | isJust t = (n, mt, ps) 
                                                  | even n = (n, mt, ps)   --even is outside
                                                  | otherwise = (n, mt, p: ps) -- odd is inside

traceAllRays :: PathTiles -> Tile -> [[Position]] -> Int
traceAllRays mpt replacement = foldl (\n ps -> n + length (rayCastLine mpt replacement ps)) 0

restrictToJustTilesOnPath :: [(Position, Tile)] -> [Position] -> PathTiles
restrictToJustTilesOnPath pts ps = PathTiles $ Map.fromList $ filter (\pt ->  fst pt `elem` ps) pts


replacementTile :: [Compass] -> Tile
replacementTile [North, South] = VPipe
replacementTile [East, West] = HPipe
replacementTile [North, East] = NEBend
replacementTile [North, West] = NWBend
replacementTile [South, West] = SWBend
replacementTile [East,South] = SEBend


findReplacement :: [(Position, Tile)] -> Tile
findReplacement ps = replacementTile (sort $ map fst ds)
    where
        start = startingPosition ps
        ds = directionOfFirstStep ps start


countUpInterior :: [(Position, Tile)] -> Int
countUpInterior pss = traceAllRays mpt replacement grid
    where
        path = findPathFromStart pss
        mpt = restrictToJustTilesOnPath pss path
        grid = createPositionGrid pss
        replacement = findReplacement pss

runPt2 =  countUpInterior . convertToPositionList <$> readData