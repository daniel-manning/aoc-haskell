module Day09_2020
    (
    ) where

    import Data.List (sort, group, nub)

    gapsBetweenData :: [Int] -> [Int]
    gapsBetweenData [x,y] = [y-x]
    gapsBetweenData (x:y:xs) = (y-x) : gapsBetweenData (y:xs)

    --(\x -> (1 + fst.head x) * (1 + fst.last x)) $
    day10Pt1 =  map (\x -> (length x, nub x)) . group . sort . gapsBetweenData . sort <$> readNumbers


    readNumbers :: IO [Int]
    readNumbers = map read . lines <$> readFile "resource/2020/day10"