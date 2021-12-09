module Day09_2021 where

import Data.List (all, (\\), nub, sort)

data Position = Position Int Int deriving (Eq, Show)
data Dimensions = Dimensions Int Int

split :: String -> [Int]
split = map (read . (:[]))

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day09"

readAndParse :: IO [[Int]]
readAndParse = map split <$> readData
-----------------

get :: Position -> [[a]] -> a
get (Position x y) as = (as !! y) !! x

neighbourhood :: Dimensions -> Position -> [Position]
neighbourhood (Dimensions mx my) (Position x y) = filter (\(Position a b) -> (a>= 0) && (b >= 0) && (a < mx) && (b < my)) [Position x (y - 1), Position x (y + 1), Position (x - 1) y, Position  (x + 1) y]

getNeighbourhoodData :: Dimensions -> Position -> [[a]] -> [a]
getNeighbourhoodData d p as = map (`get` as) (neighbourhood d p)

getNeighbourhoodDataWithPoints :: Dimensions -> Position -> [[a]] -> [(Position, a)]
getNeighbourhoodDataWithPoints d p as = map (\p -> (p, get p as)) (neighbourhood d p)

isLowestInNeighbourhood :: Ord a => Dimensions -> Position -> [[a]] -> Bool
isLowestInNeighbourhood d p as = all (> x) $ getNeighbourhoodData d p as
    where
        x = get p as

--assume non-empty arrays
lowestPoints :: Ord a => [[a]] -> [Position]
lowestPoints as = filter (\p -> isLowestInNeighbourhood (Dimensions mx my) p as) [Position x y | x <- [0.. (mx - 1)], y <- [0.. (my - 1)]]
    where
        mx = length (head as)
        my = length as

lowestPointValues :: Ord a => [[a]] -> [a]
lowestPointValues as = map (`get` as) $ lowestPoints as

riskLevel :: Int -> Int
riskLevel n =  n + 1

runPt1 :: [[Int]] -> Int
runPt1 = sum . map riskLevel . lowestPointValues
---------------------
search :: Dimensions -> Position -> [[Int]] -> [Position]
search d p as | isBlock = []
              | otherwise = nub $ map fst $ filter (\l -> snd l /= 9) $ getNeighbourhoodDataWithPoints d p as
    where
        isBlock :: Bool
        isBlock = get p as == 9

gather :: Dimensions -> [Position] -> [Position] -> [[Int]] -> [Position]
gather d searched new as | null ng = nub searched
                         | otherwise = gather d (ng ++ searched) ng as
    where
        ng = nub $ concatMap (\p -> search d p as) new \\ searched

visited :: Dimensions -> [[Position]] -> [Position] -> [[Int]] -> [[Position]]
visited _ vs [] _ = vs
visited d vs (l:left) as = visited d (c:vs) (left \\ c) as
    where
        c = gather d [] [l] as

dimensions :: [[a]] -> Dimensions
dimensions as = Dimensions (length (head as)) (length as)

spacesWithoutWalls :: (Eq a, Num a) => Dimensions -> [[a]] -> [Position]
spacesWithoutWalls (Dimensions mx my) as = filter (\p -> get p as /= 9) [Position x y | x <- [0.. (mx - 1)], y <- [0.. (my - 1)]]

runPt2 :: [[Int]] -> Int
runPt2 as = product $ take 3 $ reverse $ sort $ map length $ visited d [] (spacesWithoutWalls d as) as
    where
        d = dimensions as

solution :: IO Int
solution =  runPt2 <$> readAndParse