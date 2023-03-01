module Day03_2017 (
) where

import Data.Tuple.Extra (fst3)


data Direction = R | L | U | D deriving Show

distance (x, y) = abs x + abs y

diagonals =  (\n -> [(4*n*n - 2*n + 1, (n, n), L), (4*n*n + 1, (-n,n), D), (4*n*n + 2*n + 1, (-n, -n), R), ((2*n+1)*(2*n+1), (n, -n), R)]) =<< [1..]

closestDiagonal m = last $ zip [1..] $ takeWhile (\n -> fst3 n < m) diagonals

walk :: Int -> (Int, Int) -> Direction -> (Int, Int)
walk n (x, y) R = (x + n, y)
walk n (x, y) L = (x - n, y)
walk n (x, y) U = (x, y + n)
walk n (x, y) D = (x, y - n)

walkExtraDistance :: Int -> (Int, (Int, (Int, Int), Direction)) -> (Int, Int)
walkExtraDistance d (m, (n, pos, dir)) | m `mod` 4 == 0 = walk (d-n-1) (walk 1 pos R) U
                                       | otherwise  = walk (d-n) pos dir

findPosition 1 = (0,0)
findPosition n = walkExtraDistance n $ closestDiagonal n

findDistance = distance . findPosition
----------------------------
directions :: [Direction]
directions = R : U : L : D : directions

steps = (\n -> [n, n]) =<< [1..]

spiralDirections = concat $ zipWith replicate steps directions

fillUp n= foldl checkNeihbourhoodValues ((0,0), [((0,0), 1)]) (take n spiralDirections)

checkNeihbourhoodValues :: ((Int, Int), [((Int, Int), Int)]) -> Direction -> ((Int, Int), [((Int, Int), Int)])
checkNeihbourhoodValues (pos, neighbourValues) dir = (pos', (pos', v) : neighbourValues)
    where
        pos' = walk 1 pos dir
        v = sum $ findNValues pos' neighbourValues

findNValues :: (Int, Int) -> [((Int, Int), Int)] -> [Int]
findNValues (x, y) vs = map snd $ filter (\v -> fst v  `elem` ns) vs
    where
        ns = [(x-1, y+1), (x, y + 1), (x + 1, y + 1), 
              (x - 1, y),      (x + 1, y),
              (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]

reverseAndDrain = map snd . reverse . snd . fillUp

findFirstLargest = head . dropWhile (<361527) . reverseAndDrain