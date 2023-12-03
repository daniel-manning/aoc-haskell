module Day03_2023 (
) where

import Text.ParserCombinators.Parsec
    ( digit, letter, string, char, anyChar, parse, try, (<|>), Parser, many1 )
import Data.Either.Combinators (fromRight')
import ListUtils (convertToPositionList, Position (..))
import Grid (neighbourhood, dimensions, Dimensions(..), neighbourhoodOnInfGrid)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, insert, empty, union, map, toList, intersection, disjoint)
import Data.List (groupBy)


data Tile = Empty | Number String | Symbol Char deriving Show

parseNumber :: Parser Tile
parseNumber =  do
    n <- digit
    return $ Number [n]

parseEmpty  :: Parser Tile
parseEmpty = do
        char '.'
        return Empty

parseSymbol  :: Parser Tile
parseSymbol = do Symbol <$> anyChar

parseTile = try parseNumber <|> try parseEmpty <|> try parseSymbol

parseGrid = many1 parseTile

readData :: IO [[Tile]]
readData = map (fromRight' . parse parseGrid "") . lines <$> readFile "resource/2023/day03"

regroup :: [(Position, b)] -> [[(Position, b)]]
regroup = groupBy (\(Position _ a, _) (Position _ b, _) -> a == b)

squashNumbers :: [[Tile]] -> [[(Tile, Set Position)]]
squashNumbers ts = map (squash (Nothing, [])) $ regroup $ convertToPositionList ts

squash :: (Maybe (Tile, Set Position), [(Tile, Set Position)]) -> [(Position, Tile)] -> [(Tile, Set Position)]
squash (Just (t, ps), ts) [] = (t, ps) : ts
squash (Nothing, ts) [] = ts
squash (Just (t, pset), ts) ((ps, Empty) : xs) =  squash (Nothing, (t, pset) : ts) xs 
squash (Nothing, ts) ((ps, Empty) : xs) =  squash (Nothing, ts) xs
squash (Just (t, pset), ts) ((ps, Symbol s ) : xs) =  squash (Nothing, (Symbol s, Set.fromList [ps]) : (t, pset) : ts) xs
squash (Nothing, ts) ((ps, Symbol s ) : xs) =  squash (Nothing, (Symbol s, Set.fromList [ps]) : ts) xs
squash (Just (Number m, pset), ts) ((ps, Number n ) : xs) =  squash (Just (Number (m ++ n), Set.insert ps pset), ts) xs
squash (Nothing, ts) ((ps, Number n ) : xs) =  squash (Just (Number n, Set.fromList [ps]), ts) xs

tiles = squashNumbers <$> readData

symbols :: [[(Tile, Set Position)]] -> [(Tile, Set Position)]
symbols ts = [t | t@(Symbol _, _) <- concat ts]

numbers :: [[(Tile, Set Position)]] -> [(Tile, Set Position)]
numbers ts = [t | t@(Number _, _) <- concat ts]

findAdjacentNumbers :: [[(Tile, Set Position)]] -> [Tile]
findAdjacentNumbers ts = map fst adjacentTiles
    where
        s = foldl Set.union Set.empty $ map snd $ symbols ts
        symbolNeighbours = Set.fromList $ concat $ Set.toList $ Set.map neighbourhoodOnInfGrid s
        ns = numbers ts
        adjacentTiles = filter (\(t, ps) -> not (Set.disjoint ps symbolNeighbours)) ns



runPt1 = sum . map (\(Number n) -> read n :: Int) . findAdjacentNumbers <$> tiles