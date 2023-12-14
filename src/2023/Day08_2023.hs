{-# LANGUAGE TupleSections #-}

module Day08_2023 (
) where

import Data.Maybe (mapMaybe, fromJust, isJust)
import Data.List (sort, group, isInfixOf)
import Data.Map (Map)
import qualified Data.Map.Strict as Map (fromList, lookup, keys)
import Text.ParserCombinators.Parsec
    ( digit, string, letter, parse, (<|>), Parser, many, many1, sepBy1, alphaNum )
import Data.Either.Combinators
import Data.List.Utils (endswith)
import Data.List.Extra (foldl1')

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
    node <- many1 alphaNum
    string " = ("
    leftPathNode <- many1 alphaNum
    string ", "
    rightPathNode <- many1 alphaNum
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
---------------------

startNodes :: Map Node Paths -> [(Node, Maybe Int)]
startNodes = map (, Nothing) . filter (\(Node n) -> endswith "A" n) . Map.keys


--find the frequencies of each walk
walkFromNodeToNode' :: [(Node, Maybe Int)] -> Int -> [Direction] -> Map Node Paths -> [Int]
walkFromNodeToNode' s n ds mnp | finished s  = map (fromJust . snd) s
                               | otherwise = walkFromNodeToNode' s' (n+1) (tail ds) mnp
    where
        s' = map (\(ss, m) -> if isJust m then (ss, m) else (stepAlongPath (head ds) ss mnp, reachedEnd ss)) s
        reachedEnd (Node n') = if endswith "Z" n' then Just n else Nothing
        finished = all (\(_, m) -> isJust m)

smallestCommonMultiple :: [Int] -> Int
smallestCommonMultiple = foldl1' lcm
--walk is finished on a multiple of each frequency
runPt2 = (\(ds, mnp) -> smallestCommonMultiple (walkFromNodeToNode' (startNodes mnp) 0 (cycle ds) mnp)) <$> readData