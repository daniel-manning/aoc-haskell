module ListUtils
    (
     convertToPositionList
    ) where

    data Position = Position Int Int deriving Show

    convertToPositionList :: [[a]] -> [(Position, a)]
    convertToPositionList list = (\l -> map (\x -> (Position (fst x) (fst l), snd x)) $ zip [0..] (snd l))  =<< zip [0..] list