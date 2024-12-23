module Day09_2024 (
    runPt1, runPt2
) where

import Data.List

data Block = File {id :: Int,  length :: Int, moved :: Bool } | Free Int deriving Show

buildBlocks :: [Char] -> [Block]
buildBlocks = buildBlocks' . zip [1..]
    where
        buildBlocks' :: [(Int, Char)] -> [Block]
        buildBlocks' [] = []
        buildBlocks' [(l, x)] = [File (l `div` 2) (read [x]) False]
        buildBlocks' ((l,x): (_,y): xs) = [ File (l `div` 2) (read [x]) False, Free (read [y])] ++ buildBlocks' xs

readData :: IO [Block]
readData = buildBlocks <$> readFile "resource/2024/day09"
--------------------------------------

findMoveableEnd :: Block -> ([Block], [Block]) -> ([Block], [Block])
findMoveableEnd (Free n) (a, []) = (a, [])
findMoveableEnd (Free n) (a, xs) = case last xs of
                                    Free _ -> findMoveableEnd (Free n) (a, init xs) -- throw it away don't need it
                                    File id m m' ->  if m == n then (a ++ [File id m m'], init xs)
                                                  else if m > n then (a ++ [File id n m'], (init xs) ++ [File id (m - n) m'])
                                                  else findMoveableEnd (Free (n - m)) (a ++ [File id m m'], init xs)

reduceFileBlocks :: [Block] -> [Block]
reduceFileBlocks [] = []
reduceFileBlocks [Free n] = [Free n]
reduceFileBlocks ((File id n m):xs) = (File id n m) : reduceFileBlocks xs
reduceFileBlocks ((Free n):xs) = (\(a, b) -> a ++ reduceFileBlocks b) $ (findMoveableEnd (Free n) ([], xs))

addCheckSum :: (Integer, Integer) -> Block -> (Integer, Integer)
addCheckSum (n, t) (Free l) = (n + toInteger l, t)
addCheckSum (n, t) (File id l _) = (n + toInteger l, t + (sum $ map ((toInteger id)*) $ take l $ [n, n+1 ..]))

checkSum :: [Block] -> Integer
checkSum s = sum $ foldl' addCheckSum (0, 0) s

runPt1 :: IO Integer
runPt1 = checkSum  . reduceFileBlocks <$> readData
-------------------------------------------
isFreeSpaceAtLeastN :: Int -> Block -> Bool
isFreeSpaceAtLeastN _ (File _ _ _) = False
isFreeSpaceAtLeastN n (Free l) = n <= l

alterBlocks :: [Block] -> Block -> [Block]
alterBlocks [] _ = []
alterBlocks ((Free n):bs) (File id l _) | l == n = (File id l True): bs
                                        | otherwise = (File id l True): (Free (n-l)) : bs

consolidateFreeBlocks :: [Block] -> [Block]
consolidateFreeBlocks [] = []
consolidateFreeBlocks ((Free n): (Free m): bs) = consolidateFreeBlocks ((Free (n+m)): bs)
consolidateFreeBlocks (a:bs) = a : consolidateFreeBlocks bs

findPlaceToMove :: Block -> [Block] -> ([Block], [Block])
findPlaceToMove (Free l) bs = (bs, [Free l])
findPlaceToMove (File id l True) bs = (bs, [(File id l True)])
findPlaceToMove (File id l False) bs | null after = (bs, [(File id l False)])
                                     | otherwise = (consolidateFreeBlocks (before ++ after'), [Free l])
    where
        before = takeWhile (not . isFreeSpaceAtLeastN l) bs
        after = dropWhile (not . isFreeSpaceAtLeastN l) bs
        after' = alterBlocks after (File id l False)


reduceByMovingFiles :: [Block] -> [Block]
reduceByMovingFiles bs = reduceByMovingFiles' (bs, [])
    where
        reduceByMovingFiles' :: ([Block], [Block]) -> [Block]
        reduceByMovingFiles' (bs, rest) | null bs = rest
                                        | otherwise = reduceByMovingFiles' (bs', consolidateFreeBlocks(rs ++ rest))
            where
                (bs', rs) = findPlaceToMove (last bs) (init bs)

runPt2 :: IO Integer
runPt2 = checkSum . reduceByMovingFiles <$> readData
