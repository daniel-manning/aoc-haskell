module Day09_2015
    (
    day09Pt1
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List (nub, permutations, find, sort)
    import Data.Maybe (fromJust, isJust)

    data Edge = Edge String String Int deriving Show

    parseEdge :: Parser Edge
    parseEdge = do
            a <- many1 letter
            string " to "
            b <- many1 letter
            string " = "
            distance <- many1 digit
            return $ Edge a b (read distance)

    edges :: IO [Edge]
    edges = map (fromRight' . (parse parseEdge "")) <$> readEdges

    vertices :: [Edge] -> [String]
    vertices edgeList = nub $ sort $ (\(Edge a b _ ) -> [a,b]) =<< edgeList

    solve = permutations . vertices

    resolvePath :: [String] -> [Edge] -> [Maybe Edge]
    resolvePath [a,b] edges = [find (\(Edge x y _ ) -> (x == a && y ==b) || (y == a && x ==b) ) edges]
    resolvePath (a:b:xs) edges = find (\(Edge x y _ ) -> (x == a && y ==b) || (y == a && x ==b) ) edges : resolvePath (b:xs) edges

    totalDistance :: [Edge] -> Int
    totalDistance edgeList = sum $ map (\(Edge _ _ n) -> n) edgeList

    day09Pt1 = (\e -> minimum . map totalDistance . map fromJust . filter isJust . map sequence $ map (\p -> resolvePath p e) (solve e)) <$> edges
    day09Pt2 = (\e -> maximum . map totalDistance . map fromJust . filter isJust . map sequence $ map (\p -> resolvePath p e) (solve e)) <$> edges

    readEdges :: IO [String]
    readEdges = lines <$> readFile "resource/2015/day09"

