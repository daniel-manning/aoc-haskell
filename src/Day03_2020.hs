module Day03_2020
    (
    runSlope
    ) where

    import Data.List (find)
    import Data.Maybe (isJust)

    data Position = Position Int Int deriving (Show, Eq)
    data Gradient = Gradient Int Int deriving Show
    data SlopeDimensions = SlopeDimensions Int Int deriving Show

    travelDownSlope :: Gradient -> [String] -> Int
    travelDownSlope (Gradient x y) input = length $ filter isJust [find (\n -> n == Position ((k*x) `mod` l) (k*y)) trees | k <- [0..(h `div` y)]]
      where
        SlopeDimensions l h = sizeOfSlope input
        trees = constructTreeField input

    runSlope :: IO Int
    runSlope = travelDownSlope (Gradient 3 1) <$> readFileOfPasswords

    constructTreeField :: [String] -> [Position]
    constructTreeField input = map fst $ filter (\n -> snd n == '#') $ concat $ map (\n -> (map (\l -> ( Position (fst l) (fst n), snd l)) $ zip [0..] $ snd n) ) $ zip [0..] input

    sizeOfSlope :: [String] -> SlopeDimensions
    sizeOfSlope input = SlopeDimensions (length $ head input) (length input)

    readFileOfPasswords :: IO [String]
    readFileOfPasswords = lines <$> readFile "resource/2020/day03"