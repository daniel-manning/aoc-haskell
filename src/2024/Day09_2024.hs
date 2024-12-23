module Day09_2024 (
) where

import Data.List

data Block = File {id :: Int,  length :: Int } | Free Int deriving Show

buildBlocks :: [Char] -> [Block]
buildBlocks = buildBlocks' . zip [1..]
    where
        buildBlocks' :: [(Int, Char)] -> [Block]
        buildBlocks' [] = []
        buildBlocks' [(l, x)] = [File (l `div` 2) (read [x])]
        buildBlocks' ((l,x): (_,y): xs) = [ File (l `div` 2) (read [x]), Free (read [y])] ++ buildBlocks' xs

readData :: IO [Block]
readData = buildBlocks <$> readFile "resource/2024/day09"
--------------------------------------

findMoveableEnd :: Block -> ([Block], [Block]) -> ([Block], [Block])
findMoveableEnd (Free n) (a, []) = (a, [])
findMoveableEnd (Free n) (a, xs) = case last xs of
                                    Free _ -> findMoveableEnd (Free n) (a, init xs) -- throw it away don't need it
                                    File id m ->  if m == n then (a ++ [File id m], init xs)
                                                  else if m > n then (a ++ [File id n], (init xs) ++ [File id (m - n)])
                                                  else findMoveableEnd (Free (n - m)) (a ++ [File id m], init xs)

reduceFileBlocks :: [Block] -> [Block]
reduceFileBlocks [] = []
reduceFileBlocks [Free n] = [Free n]
reduceFileBlocks ((File id n):xs) = (File id n) : reduceFileBlocks xs
reduceFileBlocks ((Free n):xs) = (\(a, b) -> a ++ reduceFileBlocks b) $ (findMoveableEnd (Free n) ([], xs))

addCheckSum :: (Integer, Integer) -> Block -> (Integer, Integer)
addCheckSum (n, t) (Free l) = (n + toInteger l, t)
addCheckSum (n, t) (File id l) = (n + toInteger l, t + (sum $ map ((toInteger id)*) $ take l $ [n, n+1 ..]))

checkSum :: [Block] -> Integer
checkSum s = sum $ foldl' addCheckSum (0, 0) s

runPt1 :: IO Integer
runPt1 = checkSum  . reduceFileBlocks <$> readData
-------------------------------------------

