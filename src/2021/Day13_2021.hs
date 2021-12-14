module Day13_2021 where

import ListUtils ( groupBetweenBlankLines )
import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, string, (<|>))
import Data.Either.Combinators ( fromRight' )
import Data.List (nub)
    
data Point = Point Int Int deriving (Eq, Show)
data Fold = XFold Int | YFold Int deriving Show
data Dimensions = Dimensions Int Int deriving Show
data Paper = Paper Dimensions [Point] deriving Show

parsePoints :: Parser Point
parsePoints = do
    x <- many1 digit
    char ','
    y <- many1 digit
    return $ Point (read x) (read y)

parseFold :: Parser Fold
parseFold = do
    string "fold along "
    xOry <- char 'x' <|> char 'y'
    char '='
    n <- many1 digit
    return $ if xOry == 'x' then XFold (read n) else YFold (read n)

parseData :: [String] -> ([Point], [Fold])
parseData = (\x -> (map (fromRight' . parse parsePoints "") (head x), map (fromRight' . parse parseFold "") (reverse $ head $ tail x))) . groupBetweenBlankLines

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day13"

readAndParse :: IO ([Point], [Fold])
readAndParse = parseData <$> readData
--------------------
reflectPoint :: Fold -> Point -> Point
reflectPoint (XFold n) (Point x y) = if x < n then Point x y else Point (2*n - x)  y
reflectPoint (YFold n) (Point x y) = if y < n then Point x y else Point x  (2*n - y)

runFold :: Fold -> [Point] -> [Point]
runFold f = nub . map (reflectPoint f)

foldDimensions :: Dimensions -> Fold -> Dimensions
foldDimensions (Dimensions x y) (XFold n) = Dimensions ((x-1) `div` 2) y
foldDimensions (Dimensions x y) (YFold n) = Dimensions x ((y-1) `div` 2)

foldUp :: Dimensions -> [Fold] -> [Point] -> Paper
foldUp d folds points = Paper (foldl foldDimensions d folds) (foldl (flip runFold) points folds)

showPoints :: Paper -> String
showPoints (Paper (Dimensions xMax yMax) ps) = unlines [ [if Point x y `elem` ps then '#' else '.' | x <- [0..xMax] ] | y <- [0..yMax] ]

dimensions :: [Point] -> Dimensions
dimensions ps = Dimensions  xMax yMax
    where 
        xMax = maximum (map (\(Point x y) -> x) ps)
        yMax = maximum (map (\(Point x y) -> y) ps)

runPt1 :: ([Point], [Fold]) -> Int
runPt1 = (\(Paper _ ps) -> length ps) . (\l -> foldUp (dimensions (fst l)) (take 1 $ snd l) (fst l))

runPt2 :: ([Point], [Fold]) -> String
runPt2 = showPoints . (\l -> foldUp (dimensions (fst l)) (snd l) (fst l))

solution :: IO ()
solution =   putStr . runPt2 =<< readAndParse
