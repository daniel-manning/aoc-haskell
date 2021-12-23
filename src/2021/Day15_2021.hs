module Day15_2021 (solution) where

import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.List (find, (\\), minimumBy)
import Data.Function ( on ) 
import Data.Set (Set, fromList, intersection, delete)
import qualified Data.Set as Set (map, filter)
import qualified Data.Map.Strict as Map
import qualified Data.OrdPSQ as PSQ
import Data.Tuple.Extra (fst3)


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

startingUnvisited :: Position -> PSQ.OrdPSQ Position Risk Position
startingUnvisited (Position maxX maxY) = foldl (\b a -> PSQ.insert a Infinity a b) PSQ.empty [Position x y | x <- [0..maxX], y <- [0..maxY]]


tentativeDistances :: Position -> PSQ.OrdPSQ Position Risk Position
tentativeDistances = PSQ.insert (Position 0 0) (Risk 0) (Position 0 0) . startingUnvisited 

markUpNodeDistances :: Position -> PSQ.OrdPSQ Position Risk Position -> [[Int]] -> PSQ.OrdPSQ Position Risk Position
markUpNodeDistances current nodeRisks xs | current == finish = updatedRisks
                                                   | otherwise = markUpNodeDistances newCurrent newUnvisited xs
    where
        finish = endPosition xs
        unvisitedNeighbours = Set.filter (`PSQ.member` nodeRisks) $ nextStep finish xs current
        currentNodeRisk = fst $ fromJust $ PSQ.lookup current nodeRisks
        updatedRisks = foldl (\b a -> updateRisk currentNodeRisk (get a xs) b a) nodeRisks unvisitedNeighbours
        newUnvisited = PSQ.delete current updatedRisks
        newCurrent = fst3 $ fromJust $ PSQ.findMin newUnvisited 

updateRisk :: Risk -> Int -> PSQ.OrdPSQ Position Risk Position -> Position -> PSQ.OrdPSQ Position Risk Position
updateRisk Infinity _ _ _ = error "Why is current node unset?"
updateRisk (Risk n) edge risks position = PSQ.insert position (min oldRisk newRisk) position risks
    where
        newRisk = Risk (n + edge)
        oldRisk = fst $ fromMaybe (newRisk, position) $ PSQ.lookup position risks
        

startSearch :: [[Int]] -> PSQ.OrdPSQ Position Risk Position
startSearch xs = markUpNodeDistances startPosition (tentativeDistances finish) xs
    where
        finish = endPosition xs

minimumRisk :: [[Int]] -> Risk
minimumRisk xs = fst $ fromJust $ PSQ.lookup (endPosition xs) $ startSearch xs

runPt1 :: [[Int]] -> Risk
runPt1 = minimumRisk

{--
PT1 - with Map for risking:
real	0m20.454s
user	0m33.995s
sys	0m16.447s
--}

{--
PT1 - with priority search queue
real	0m0.411s
user	0m0.413s
sys	0m0.072s
--}

---------------------------
repeatBlock :: Int -> Int -> (Int -> Int -> Int) -> [[Int]] -> [[Int]]
repeatBlock n m f xs = foldl (\b a -> b ++ map (doAndSum n a f) xs) [] [1..n]

doAndSum :: Int -> Int -> (Int -> Int -> Int) -> [Int] -> [Int]
doAndSum n m f x = foldl (\b a -> b ++ map (f (a+m)) x) (map (f m) x) [1..(n-1)]

modWrap :: Int -> Int -> Int
modWrap n m | n + m - 1 <= 9 = n + m - 1
            | otherwise = (n + m - 1) `mod` 9

createFullGrid :: [[Int]] -> [[Int]]
createFullGrid = repeatBlock 5 5 modWrap 

runPt2 :: [[Int]] -> Risk
runPt2 = minimumRisk . createFullGrid

{--
real	0m9.386s
user	0m11.593s
sys	0m2.241s
--}

solution :: IO Risk
solution = runPt2 <$> readAndParse
