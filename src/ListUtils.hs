module ListUtils
    (
     convertToPositionList,
     window,
     Position(..)
    ) where

    import Data.Hashable

    data Position = Position Int Int deriving (Eq, Show)

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