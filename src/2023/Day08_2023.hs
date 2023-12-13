module Day08_2023 (
) where

import Data.Maybe (mapMaybe, fromJust)
import Data.List (sort, group, isInfixOf)
import Data.Map (Map)
import qualified Data.Map.Strict as Map (fromList, lookup)
import Text.ParserCombinators.Parsec
    ( digit, string, letter, parse, (<|>), Parser, many, many1, sepBy1 )
import Data.Either.Combinators

data Direction = R | L deriving Show

data Paths = Paths Node Node deriving Show
newtype Node = Node String deriving (Eq, Ord, Show)
--------------

parseDirection :: Char -> Maybe Direction
parseDirection 'R' = Just R
parseDirection 'L' = Just L
parseDirection _ = Nothing

parseStrategy :: [Char] -> [Direction]
parseStrategy = mapMaybe parseDirection

parsePaths :: Parser (Node, Paths)
parsePaths = do
    node <- many1 letter
    string " = ("
    leftPathNode <- many1 letter
    string ", "
    rightPathNode <- many1 letter
    string ")"
    return (Node node, Paths (Node leftPathNode) (Node rightPathNode))

parseNetwork :: [String] -> Map Node Paths
parseNetwork = Map.fromList . map (fromRight' . parse parsePaths "")

parseStrategyAndNetwork :: [String] -> ([Direction], Map Node Paths)
parseStrategyAndNetwork xs = (parseStrategy (head xs), parseNetwork (drop 2 xs))

readData :: IO ([Direction], Map Node Paths)
readData = parseStrategyAndNetwork . lines <$> readFile "resource/2023/day08"
-----------------
choosePath :: Direction -> Paths -> Node
choosePath R (Paths _ n) = n
choosePath L (Paths n _) = n

stepAlongPath :: Direction -> Node -> Map Node Paths -> Node
stepAlongPath d n mnp = choosePath d nextPath
    where
        nextPath = fromJust $ Map.lookup n mnp

walkFromNodeToNode :: Node -> Node -> Int -> [Direction] -> Map Node Paths -> Int
walkFromNodeToNode s d n ds mnp | s == d  = n
                             | otherwise = walkFromNodeToNode s' d (n+1) (tail ds) mnp
    where
        s' = stepAlongPath (head ds) s mnp

runPt1 = (\(ds, mnp) -> walkFromNodeToNode (Node "AAA") (Node "ZZZ") 0 (cycle ds) mnp) <$> readData

