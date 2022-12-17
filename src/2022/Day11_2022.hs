module Day11_2022 (
    runPt2
) where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, string, sepBy1, alphaNum, char, space)
import Data.Either.Combinators (fromRight')
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.List (sortBy, groupBy, foldl', sort)
import Data.List.Extra (trim)

data Monkey = Monkey {
    label :: Int,
    items :: [Int],
    operation :: Operation,
    test :: Test,
    inspections :: Int
} deriving Show

parseLabel :: Parser Int
parseLabel = do
    string "Monkey "
    n <- many1 digit
    string ":"
    return $ read n

parseItemList :: Parser [Int]
parseItemList = do
    string "  Starting items: "
    xs <- sepBy1 (many1 digit) (string ", ")
    return $ map read xs

data Term = Old | N Int deriving Show
data Op = Add | Multiply deriving Show

parseOp '+' = Add
parseOp '*' = Multiply 

parseTerm " old" = Old
parseTerm c = N (read $ trim c)

data Operation = Operation Term Op Term deriving Show
parseOperation :: Parser Operation
parseOperation = do
    string "  Operation: new = old "
    c <- char '+' <|> char '*'
    r <- many1 (alphaNum <|> space)
    return (Operation Old (parseOp c) (parseTerm r))

groupBetweenBlankLines :: Foldable t => [t a] -> [[t a]]
groupBetweenBlankLines input = map reverse $ groupBetweenBlankLines'' input []
 where
    groupBetweenBlankLines'' [] n = [n]
    groupBetweenBlankLines'' (x:xs) n | null x = n : groupBetweenBlankLines'' xs []
                                      | otherwise = groupBetweenBlankLines'' xs (x : n)

data Test = Test Int Int Int deriving Show
parseTest [p, t, f] = Test predicate trueMonkey falseMonkey
    where
        predicate = read $ trim $ drop 21 p
        trueMonkey = read $ trim $ drop 29 t
        falseMonkey = read $ trim $ drop 30 f


makeMonkey :: [String] -> Monkey
makeMonkey xs  = Monkey label items operation test 0
    where
        label = fromRight' $ parse parseLabel "" (head xs)
        items = fromRight' $ parse parseItemList "" (xs !! 1)
        operation = fromRight' $ parse parseOperation "" (xs !! 2)
        test = parseTest $ take 3 $ drop 3 xs


readData :: IO [[String]]
readData = groupBetweenBlankLines . lines <$> readFile "resource/2022/day11"

readAndParse = map makeMonkey <$> readData
-----------------------

applyOperation :: Operation -> Int -> Int
applyOperation (Operation Old Add (N n)) x = x + n
applyOperation (Operation Old Multiply (N n)) x = x * n
applyOperation (Operation Old Multiply Old) x = x * x

applyTest :: Test -> Int -> Int
applyTest (Test n tm fm) x | x `mod` n == 0 = tm
                           | otherwise      = fm

inspectPackage :: Operation -> Test -> Int -> (Int, Int)
inspectPackage op t n = (m, n'')
    where
        n' = applyOperation op n
        n'' = n' `div` 3
        m = applyTest t n''

monkeyInspectsPackages :: Monkey -> [(Int, Int)]
monkeyInspectsPackages (Monkey _ is op t _) = map (inspectPackage op t) is

collectChanges :: (Ord a, Eq a) => [(a, b)] -> [(a, [b])]
collectChanges = map (\xs -> (head $ map fst xs , map snd xs)) . groupBy (\a b -> fst a == fst b) . sortBy (compare `on` fst)

findMonkey :: [Monkey] -> Int -> Monkey
findMonkey ms n = head $ filter (\m -> label m == n) ms

updateMonkeyItems :: [Monkey] -> (Int, [Int]) -> [Monkey]
updateMonkeyItems ms (n, is) = m { items = items m ++ is} : filter (\mk -> label mk /= n) ms
    where
        m = findMonkey ms n

removeMonkeyItems :: [Monkey] -> Int -> [Monkey]
removeMonkeyItems ms n = m { items = [], inspections = (inspections m) + length (items m)} : filter (\mk -> label mk /= n) ms
    where
        m = findMonkey ms n

updatePackagesInMonkeys :: [Monkey] -> Int -> [(Int, Int)] -> [Monkey]
updatePackagesInMonkeys ms n nps = foldl' updateMonkeyItems ms' changes
    where
       ms' = removeMonkeyItems ms n
       changes = collectChanges nps

turn :: [Monkey] -> Int -> [Monkey]
turn ms n =  updatePackagesInMonkeys ms n cs
    where
        m = findMonkey ms n
        cs = monkeyInspectsPackages m
       
monkeyRound :: [Monkey] -> [Monkey]
monkeyRound ms = foldl' turn ms [0..(length ms -1)]

monkeyBusiness :: [Monkey] -> Int
monkeyBusiness = (\xs -> (head xs) * (xs !! 1)) . take 2 . reverse . sort . map inspections

endOfRound :: Int -> [Monkey] -> [Monkey]
endOfRound n ms = iterate monkeyRound ms !! n


runPt1 = monkeyBusiness . endOfRound 20  <$> readAndParse
---------------

inspectPackage' :: Int -> Operation -> Test -> Int -> (Int, Int)
inspectPackage' l op t n = (m, n')
    where
        n' = applyOperation op n `mod` l -- stop worry overflowing
        m = applyTest t n'

monkeyInspectsPackages' :: Int -> Monkey -> [(Int, Int)]
monkeyInspectsPackages' l (Monkey _ is op t _) = map (inspectPackage' l op t) is

turn' :: [Monkey] -> Int -> [Monkey]
turn' ms n =  updatePackagesInMonkeys ms n cs
    where
        m = findMonkey ms n
        l = lcmT ms
        cs = monkeyInspectsPackages' l m
       
monkeyRound' :: [Monkey] -> [Monkey]
monkeyRound' ms = foldl' turn' ms [0..(length ms -1)]

endOfRound' :: Int -> [Monkey] -> [Monkey]
endOfRound' n ms = iterate monkeyRound' ms !! n

lcmT :: [Monkey] -> Int
lcmT = product . map ((\(Test a _ _) -> a) . test)

runPt2 = monkeyBusiness . endOfRound' 10000 <$> readAndParse