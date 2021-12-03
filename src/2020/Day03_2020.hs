module Day03_2020
    (
    runSlope,
    findProduct
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
    runSlope = travelDownSlope (Gradient 3 1) <$> readCourse

    findProduct :: IO Int
    findProduct = product . (\input -> map (`travelDownSlope` input) [Gradient 1 1, Gradient 3 1, Gradient 5 1, Gradient 7 1, Gradient 1 2]) <$> readCourse

    constructTreeField :: [String] -> [Position]
    constructTreeField input = map fst $ filter (\n -> snd n == '#') $ concatMap (\n -> zipWith (curry (\l -> ( Position (fst l) (fst n), snd l)) ) [0..] $ snd n) (zip [0..] input)

    sizeOfSlope :: [String] -> SlopeDimensions
    sizeOfSlope input = SlopeDimensions (length $ head input) (length input)

    readCourse :: IO [String]
    readCourse = lines <$> readFile "resource/2020/day03"