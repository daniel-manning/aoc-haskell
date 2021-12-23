module Day15_2021 (solution) where

import Data.Maybe (catMaybes, fromJust)
import Data.List (find, (\\), minimumBy)
import Data.Function ( on ) 
import Data.Set (Set, fromList, intersection, delete)
import qualified Data.Set as Set (map)
import qualified Data.Map.Strict as Map

import Debug.Trace (trace)

data Position  = Position Int Int deriving (Ord, Eq, Show)
data Risk = Infinity | Risk Int deriving (Eq, Show)

instance Ord Risk where
    compare (Risk n) (Risk m) = compare n m
    compare Infinity (Risk n) = GT
    compare (Risk n) Infinity = LT
    compare Infinity Infinity = EQ


split :: String -> [Int]
split = map (read . (:[]))

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day15"

readAndParse :: IO [[Int]]
readAndParse = map split <$> readData
----------------

maxPosition :: [[a]] -> Position
maxPosition xs = Position (length (head xs) - 1) (length xs - 1)

get :: Position -> [[a]] -> a
get (Position x y) xs = (xs !! y) !! x

nextStep :: Position -> [[Int]] -> Position -> Set Position
nextStep (Position maxX maxY) xs (Position x y) = fromList $ catMaybes [up, down, rightD, leftD]
    where
        down = if y < maxY then Just (Position x (y+1)) else Nothing
        rightD = if x < maxX then Just (Position (x+1) y) else Nothing
        up = if y > 0 then Just (Position x (y-1)) else Nothing
        leftD = if x > 0 then Just (Position (x-1) y) else Nothing

startPosition :: Position
startPosition = Position 0 0

endPosition :: Foldable t => [t a] -> Position
endPosition xs = Position (length (head xs) - 1) (length xs - 1)

startingUnvisited :: Position -> Set Position
startingUnvisited (Position maxX maxY) = fromList [Position x y | x <- [0..maxX], y <- [0..maxY]]

tentativeDistances :: Position -> Map.Map Position Risk
tentativeDistances (Position maxX maxY) = Map.update (\a -> Just (Risk 0)) startPosition $ Map.fromList [(Position x y, Infinity) | x <- [0..maxX], y <- [0..maxY]]

markUpNodeDistances :: Position -> Set Position -> Map.Map Position Risk -> [[Int]] -> Map.Map Position Risk
markUpNodeDistances current unvisited nodeRisks xs | current == finish = updatedRisks
                                                   | otherwise = {- trace (show newCurrent)  -}markUpNodeDistances newCurrent newUnvisited updatedRisks xs
    where
        finish = endPosition xs
        unvisitedNeighbours = nextStep finish xs current `intersection` unvisited
        currentNodeRisk = fromJust $ Map.lookup current nodeRisks
        updatedRisks = foldl (\b a -> updateRisk currentNodeRisk (get a xs) b a) nodeRisks unvisitedNeighbours
        newUnvisited = delete current unvisited
        newCurrent = fst $ minimumBy (compare `on` snd) $ Set.map (\p -> (p , fromJust $ Map.lookup p updatedRisks)) newUnvisited 

updateRisk :: Risk -> Int -> Map.Map Position Risk -> Position -> Map.Map Position Risk
updateRisk Infinity _ _ _ = error "Why is current node unset?"
updateRisk (Risk n) edge risks position = Map.update (`smallestRisk` newRisk) position risks
    where
        newRisk = Risk (n + edge)

smallestRisk :: Risk -> Risk -> Maybe Risk
smallestRisk a b = Just $ min a b

startSearch :: [[Int]] -> Map.Map Position Risk
startSearch xs = markUpNodeDistances startPosition (startingUnvisited finish) (tentativeDistances finish) xs
    where
        finish = endPosition xs

minimumRisk :: [[Int]] -> Risk
minimumRisk xs = fromJust $ Map.lookup (endPosition xs) $ startSearch xs

runPt1 = minimumRisk

{--
PT1:
real	0m20.454s
user	0m33.995s
sys	0m16.447s
--}

solution :: IO Risk
solution = runPt1 <$> readAndParse