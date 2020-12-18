module Day10_2020
    (
    ) where

    import Data.List (sort, group, nub)

    gapsBetweenData :: [Int] -> [Int]
    gapsBetweenData [x,y] = [y-x]
    gapsBetweenData (x:y:xs) = (y-x) : gapsBetweenData (y:xs)

    itemsThatCanBeDropped :: [Int] -> [Int]
    itemsThatCanBeDropped [x,y,z] = [y | z-x <= 3]
    itemsThatCanBeDropped (x:y:z:xs) = if z-x <= 3 then y:itemsThatCanBeDropped(y:z:xs) else itemsThatCanBeDropped(y:z:xs)

    --(\x -> (1 + fst.head x) * (1 + fst.last x)) $
    day10Pt1 =  map (\x -> (length x, nub x)) . group . sort . gapsBetweenData . sort <$> readNumbers
    day10Pt2 =  product . map (droppablePerms . fst) . filter (\n -> snd n /= 3) . map (\x -> (length x, head $ nub x)) . group . gapsBetweenData . sort <$> readNumbers

    droppablePerms 1 = 1
    droppablePerms 2 = 2
    droppablePerms 3 = 4
    droppablePerms 4 = 7

    readNumbers :: IO [Int]
    readNumbers = map read . lines <$> readFile "resource/2020/day10"