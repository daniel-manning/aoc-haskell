module Day05_2022 (
) where

import Data.List
import Data.List.Extra (trim)
import Data.List.Split (chunksOf)
import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, string)
import Data.Either.Combinators
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromJust)

data Cargo = Empty | Cargo Char deriving (Eq, Show)
data Move = Move Int Int Int deriving Show

type Stacks = IntMap.IntMap [Cargo]

--readData :: IO [[Position]]
readData = lines <$> readFile "resource/2022/day05"

groupBetweenBlankLines :: Foldable t => [t a] -> [[t a]]
groupBetweenBlankLines input = map reverse $ groupBetweenBlankLines'' input []

groupBetweenBlankLines'' :: Foldable t => [t a] -> [t a] -> [[t a]]
groupBetweenBlankLines'' [] n = [n]
groupBetweenBlankLines'' (x:xs) n | null x = n : groupBetweenBlankLines'' xs []
                                  | otherwise = groupBetweenBlankLines'' xs (x : n)

parseCargo :: String -> Cargo
parseCargo xs | null (trim xs)  = Empty
              | otherwise       = Cargo (xs !! 1)

parseRow :: String -> [Cargo]
parseRow = map (parseCargo . take 3) . chunksOf 4


parseStacks :: [String] -> [[Cargo]] -- reversed for operations
parseStacks = map (reverse . filter (/= Empty)) . transpose . map parseRow . drop 1 . reverse

turnListToMap :: [[a]] -> IntMap.IntMap [a]
turnListToMap as = IntMap.fromList (zip [1..] as)

parseCommands :: Parser Move
parseCommands = do
        string "move "
        quantity <- many1 digit
        string " from "
        columnFrom <- many1 digit
        string " to "
        columnTo <- many1 digit
        return $ Move (read quantity) (read columnFrom) (read columnTo)

parseMoves = map (fromRight' . parse parseCommands "")

--readAndParse :: IO (IntMap.IntMap [Cargo], [Move])
readAndParse =  (\xs -> (turnListToMap (parseStacks (head xs)), parseMoves (xs !! 1))) . groupBetweenBlankLines <$> readData
-----------------------

moveCargo :: Stacks -> Move -> Stacks
moveCargo cargo (Move n from to) = IntMap.insert from rest $ IntMap.insert to newTo $ IntMap.delete from $ IntMap.delete to cargo
    where
        mc = take n $ fromJust $ IntMap.lookup from cargo
        rest = drop n $ fromJust $ IntMap.lookup from cargo
        newTo = (reverse mc) ++ fromJust (IntMap.lookup to cargo)

moveEverything :: Stacks -> [Move] -> Stacks
moveEverything cargo ms = foldl' moveCargo cargo ms


readTopLineOfStack :: Stacks -> String
readTopLineOfStack = map ((\(Cargo a) -> a) . head) . IntMap.elems

runPt1 = readTopLineOfStack . uncurry moveEverything <$> readAndParse
----------------
moveCargo' :: Stacks -> Move -> Stacks
moveCargo' cargo (Move n from to) = IntMap.insert from rest $ IntMap.insert to newTo $ IntMap.delete from $ IntMap.delete to cargo
    where
        mc = take n $ fromJust $ IntMap.lookup from cargo
        rest = drop n $ fromJust $ IntMap.lookup from cargo
        newTo = mc ++ fromJust (IntMap.lookup to cargo)

moveEverything' :: Stacks -> [Move] -> Stacks
moveEverything' cargo ms = foldl' moveCargo' cargo ms

runPt2 = readTopLineOfStack . uncurry moveEverything' <$> readAndParse