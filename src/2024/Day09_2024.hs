module Day09_2024 (
) where

import Data.List
import Debug.Trace

data Block = File Int Int | Free Int deriving Show


readData = readFile "resource/2024/day09"

buildBlocks = buildBlocks' . zip [1..]

buildBlocks' :: [(Int, Char)] -> [Block]
buildBlocks' [] = []
buildBlocks' [(l, x)] = [File (l `div` 2) (read [x])]
buildBlocks' ((l,x): (_,y): xs) = [ File (l `div` 2) (read [x]), Free (read [y])] ++ buildBlocks' xs

buildFileBlocks :: [Block] -> String
buildFileBlocks [] = []
buildFileBlocks ((Free n):xs) = replicate n '.' ++ buildFileBlocks xs
buildFileBlocks ((File id n):xs) = replicate n (head $ show id) ++ buildFileBlocks xs

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

checkSum :: [Block] -> Integer
checkSum s = sum $ map (\(a, b) -> a * (read [b])) $ zip [0..] (buildFileBlocks s)

runPt1 = checkSum  . reduceFileBlocks . buildBlocks <$> readData

---instead of writing out the strings you can keep them as blocks then replace free blocks wih File blocks from the right
