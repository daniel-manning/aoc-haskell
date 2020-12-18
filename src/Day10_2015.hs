module Day10_2015
    (
    day10Pt1,
    day10Pt2
    ) where

    import Data.List (group, nub, foldl1')

    lookAndSay :: [Int] -> [Int]
    lookAndSay s = (\x -> length x : nub x) =<< group s

    applyTimes n s = last $ map length $ take (n+1) $ iterate lookAndSay s

    test = applyTimes 5 [1]
    day10Pt1 = applyTimes 40 [3,1,1,3,3,2,2,1,1,3]
    day10Pt2 = applyTimes 50 [3,1,1,3,3,2,2,1,1,3]