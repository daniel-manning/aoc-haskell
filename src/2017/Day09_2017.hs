module Day09_2017 (
) where

import Debug.Trace(trace)

readData :: IO [String]
readData = lines <$> readFile "resource/2017/day09"
---------------

data Contents = Garbage String | Group [Contents] deriving Show


takeOutGarbage :: String -> (Contents, String)
takeOutGarbage xs = takeOutGarbage' xs []
    where
        takeOutGarbage' :: String -> String -> (Contents, String)
        takeOutGarbage' ('>':as) bs = (Garbage bs, as)
        takeOutGarbage' ('!' : a : as) bs = takeOutGarbage' as bs
        takeOutGarbage' (a : as) bs = takeOutGarbage' as (bs ++ [a])

parseGroup :: String -> (Contents, String)
parseGroup ('{' : xs) = parseGroup' xs []
    where
        parseGroup' :: String -> [Contents] -> (Contents, String)
        parseGroup' ('}':as) bs = (Group bs, as)
        parseGroup' ('<':as) bs = (\l ->  parseGroup' (snd l) (bs ++ [fst l])) $ takeOutGarbage as
        parseGroup' ('{':as) bs = (\l ->  parseGroup' (snd l) (bs ++ [fst l])) $ parseGroup' as []
        parseGroup' (',':as) bs = parseGroup' as bs

countGroups :: Contents -> Int
countGroups (Garbage _) = 0
countGroups (Group gs) = 1 + sum (map countGroups gs)

groupValues :: Contents -> Int
groupValues gs = groupValues' 1 gs
    where
        groupValues' :: Int -> Contents -> Int
        groupValues' n (Garbage _) = 0
        groupValues' n (Group gs) = n + sum (map (groupValues' (n+1)) gs)

day09Pt1 = head . map (groupValues . fst . parseGroup) <$> readData
---------------
countGarbage :: Contents -> Int
countGarbage (Garbage s) = length s
countGarbage (Group gs) = sum $ map countGarbage gs

day09Pt2 = head . map (countGarbage . fst . parseGroup) <$> readData
