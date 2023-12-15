module Day09_2023 (
) where

readData :: IO [[Integer]]
readData = map (map read . words) . lines <$> readFile "resource/2023/day09"
------------------
differenceLine :: [Integer] -> [Integer]
differenceLine (x: y: xs) = (x - y) : differenceLine (y:xs)
differenceLine _ = []

finished :: [Integer] -> Bool
finished = all (==0)

makeDifferenceLines :: [Integer] -> [[Integer]]
makeDifferenceLines xs | finished xs' = [xs]
                       | otherwise = xs : makeDifferenceLines xs'
    where
        xs' = differenceLine xs

makePrediction :: [[Integer]] -> Integer
makePrediction = foldr (\ a b -> last a - b) 0 

findPrediction :: [Integer] -> Integer
findPrediction  = makePrediction . makeDifferenceLines

runPt1 = sum . map findPrediction <$> readData