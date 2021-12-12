module Day11_2021 where

import Grid ( Dimensions(..), Position(..))
import qualified Data.Set as S (Set, map, fromList, filter, elemAt, delete, insert, foldl)
import Data.List(all)
import Data.Bifunctor (Bifunctor, second)
import Debug.Trace ( trace )

data FlasherState = FlasherState Int (S.Set (Position, Int)) deriving Show


split :: String -> [Int]
split = map (read . (:[]))

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day11"

readAndParse :: IO [[Int]]
readAndParse = map split <$> readData
---------------------

turnIntoSet :: Ord b => [[b]] -> S.Set (Position, b)
turnIntoSet xs = S.fromList [ (Position x y,  (xs !! y) !! x) | x <- [0 .. (length (head xs) - 1)], y <- [0 .. (length xs - 1)]]

mapGrid :: (Ord a, Ord c) => (b -> c) -> S.Set (a, b) -> S.Set (a, c)
mapGrid f = S.map (second f)

incrementAll :: S.Set (Position, Int) -> S.Set (Position, Int)
incrementAll = mapGrid (+1)

neighbourhood :: Dimensions -> Position -> [Position]
neighbourhood (Dimensions mx my) (Position x y) = filter (\(Position a b) -> (a>= 0) && (b >= 0) && (a < mx) && (b < my)) [
    Position (x - 1) (y - 1), Position x (y - 1), Position (x+1) (y - 1),
    Position (x - 1) y,                           Position (x+1) y,
    Position (x - 1) (y + 1), Position x (y + 1), Position  (x + 1) (y+1)
    ]

getNeighbourhoodDataWithPoints :: Dimensions -> Position -> S.Set (Position, a) -> [(Position, a)]
getNeighbourhoodDataWithPoints d p as = map (\pp -> S.elemAt 0 $ S.filter (\l -> fst l == pp) as) (neighbourhood d p)

points :: FlasherState -> S.Set (Position, Int)
points (FlasherState _ s) = s

flashes :: FlasherState -> Int
flashes (FlasherState n _) = n

deletePoint :: Position -> S.Set (Position, Int) -> S.Set (Position, Int)
deletePoint p s = S.foldl (flip S.delete) s $ S.filter (\l -> fst l == p) s

flashGrid :: Dimensions -> FlasherState -> (Position, Int) -> FlasherState
flashGrid d (FlasherState n s) o = FlasherState (n + 1)  run
    where
        neighbours = getNeighbourhoodDataWithPoints d (fst o) s
        withFlashed = S.insert (fst o, 0) $ deletePoint (fst o) s
        addOneToNeighbour st (p, n) | n == 0 = st
                                    | otherwise = S.insert (p, n + 1) $ deletePoint p st
        run = foldl addOneToNeighbour withFlashed neighbours

flashOctopus :: Dimensions -> FlasherState -> FlasherState
flashOctopus d ss = foldl (flashGrid d) ss flashers
    where
        flashers = S.filter (\l -> snd l > 9) $ points ss

repeatWhileUnflashedOctopus :: Dimensions -> FlasherState -> FlasherState
repeatWhileUnflashedOctopus d (FlasherState n s) | all (\l -> snd l <= 9) s = FlasherState n s
                                                 | otherwise = repeatWhileUnflashedOctopus d (flashOctopus d (FlasherState n s))

runRound :: Dimensions -> FlasherState -> FlasherState
runRound d  (FlasherState n s) = repeatWhileUnflashedOctopus d (FlasherState n (incrementAll s))

nTimes :: Int -> (c -> c) -> c -> c
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f

runPt1 :: [[Int]] -> Int
runPt1 = flashes . nTimes 100 (runRound (Dimensions 10 10)) . (FlasherState 0 . turnIntoSet)
-------------------------------------------
haveAllFlashedAtOnce :: FlasherState -> Bool
haveAllFlashedAtOnce (FlasherState n s) = all (\l -> snd l == 0) s

runWhileUnfinished :: Int -> (a -> Bool) -> (a -> a) -> a -> Int
runWhileUnfinished n p f a | p a = n
                           | otherwise = runWhileUnfinished (n+1) p f (f a)

runPt2 :: [[Int]] -> Int
runPt2 =  runWhileUnfinished 0 haveAllFlashedAtOnce (runRound (Dimensions 10 10)) . (FlasherState 0 . turnIntoSet)

solution :: IO Int
solution = runPt2 <$> readAndParse
