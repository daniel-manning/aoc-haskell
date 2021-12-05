module Day04_2021
    (
    ) where

    import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, string)
    import Data.Either.Combinators
    import Data.Set(Set, fromList, intersection, union, empty)

    data Point = P Int Int deriving (Eq, Ord, Show)
    data Line = L Point Point deriving Show

    parseLine :: Parser Line
    parseLine = do
        x1 <- many1 digit
        char ','
        y1 <- many1 digit
        string " -> "
        x2 <- many1 digit
        char ','
        y2 <- many1 digit
        return $ L (P (read x1) (read y1)) (P (read x2) (read y2))


    readData :: IO [String]
    readData = lines <$> readFile "resource/2021/day05"

    readAndParse :: IO [Line]
    readAndParse = map (fromRight' . parse parseLine "") <$> readData
    -------------------

    isHorizontalOrVertical :: Line -> Bool
    isHorizontalOrVertical (L (P x1 y1) (P x2 y2)) = y1 == y2 || x1 == x2

    intersections :: Line -> Line -> Set Point
    intersections l1 l2 = fromList (pointSet l1) `intersection` fromList (pointSet l2)
        where
            pointSet (L (P x1 y1) (P x2 y2)) | x1 == x2 = [P x1 y | y <- [(min y1 y2)..(max y1 y2)]]
                                             | y1 == y2 = [P x y1 | x <- [(min x1 x2)..(max x1 x2)]]
                                             | otherwise = []

    overlapLines :: Set Point -> [Line] -> Set Point
    overlapLines s [] = s
    overlapLines s (x: xs) = overlapLines (foldl union s crosses) xs
        where
            crosses = map (intersections x) xs

    runPt1 :: [Line] -> Int
    runPt1 = length . overlapLines empty . filter isHorizontalOrVertical 
    --NB: Not massively efficient we could actually just work out where they overlap if anywhere
    ---------------------------------


    main =  runPt1 <$> readAndParse