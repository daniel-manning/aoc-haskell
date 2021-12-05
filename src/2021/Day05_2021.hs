module Day05_2021
    (
        solution
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
            crosses = map (inter x) xs

    runPt1 :: [Line] -> Int
    runPt1 = length . overlapLines empty . filter isHorizontalOrVertical 
    

    runPt2 = length . overlapLines empty 
    ---------------------------------

    sgn :: Int -> Int -> Int
    sgn 0 b = signum b
    sgn a 0 = signum a
    sgn a b = signum a * signum b

    inter :: Line -> Line -> Set Point
    inter (L p1 p2) (L p3 p4) = fromList [point p1 t1 | t1 <- [min 0 m1 .. max 0 m1], t2 <- [min 0 m2 .. max 0 m2], v p1 p2 p3 p4 == (t2 * fst step2 - t1 * fst step1, t2 * snd step2 - t1 * snd step1)]
        where
            v (P x1 y1) _ (P x3 y3) _ = (x1 - x3, y1 -y3)
            step p1@(P x1 y1) p2@(P x2 y2) = let k = mult p1 p2 in ((x2 - x1) `div` k, (y2 - y1) `div` k)
            mult (P x1 y1) (P x2 y2) = sgn (x2 -x1) (y2 - y1) * max (abs (x2 - x1)) (abs (y2 - y1))
            m1 = mult p1 p2
            m2 = mult p3 p4
            step1 = step p1 p2
            step2 = step p3 p4
            point (P x1 y1) n = P (x1 + n * fst step1) (y1 + n * snd step1)

    solution =  runPt2 <$> readAndParse

    {--
    Compiled time to run with stack
    real	2m3.239s
    user	2m4.738s
    sys	0m0.611s
    --}