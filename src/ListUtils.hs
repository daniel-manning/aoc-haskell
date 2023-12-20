module ListUtils
    (
     convertToPositionList,
     window,
     Position(..),
     groupBetweenBlankLines,
     createPositionGrid
    ) where

import Data.Hashable ( Hashable(hashWithSalt) )
import Models (Position(..))
import Data.List (sortBy, groupBy)



instance Hashable Position where
    hashWithSalt s (Position x y) =
        s `hashWithSalt`
        x `hashWithSalt` y

convertToPositionList :: [[a]] -> [(Position, a)]
convertToPositionList list = (\l -> map (\x -> (Position (fst x) (fst l), snd x)) $ zip [0..] (snd l))  =<< zip [0..] list

window :: Int -> [a] -> [[a]]
window n numbers | length numbers < n = [[]]
                    | length numbers == n = [numbers]
                    | otherwise = take n numbers : window n (tail numbers)

groupBetweenBlankLines :: [String] -> [[String]]
groupBetweenBlankLines input = groupBetweenBlankLines'' input []

groupBetweenBlankLines'' :: [String] -> [String] -> [[String]]
groupBetweenBlankLines'' [] n = [n]
groupBetweenBlankLines'' (x:xs) n | null x = n : groupBetweenBlankLines'' xs []
                                    | otherwise = groupBetweenBlankLines'' xs (x : n)

orderByFirst :: Position -> Position -> Ordering
orderByFirst (Position a b) (Position c d) = compare a c 

orderBySecond :: Position -> Position -> Ordering
orderBySecond (Position a b) (Position c d) = compare b d 

createPositionGrid :: [(Position, a)] -> [[Position]]
createPositionGrid pts = map (sortBy orderByFirst) $ groupBy (\(Position a b) (Position c d) -> b == d)  $ sortBy orderBySecond ps
    where
        ps = map fst pts 