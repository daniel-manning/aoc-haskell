module Day05_2024 (
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import Data.List (intersect,sortBy)
import ListUtils (groupBetweenBlankLines)
import Data.Set (Set)
import qualified Data.Set as Set

data PageOrderRule = PageOrderRule Int Int deriving (Eq, Show)
data Update = Update [Int] deriving Show
-----------
parsePageOrderRule :: Parser PageOrderRule
parsePageOrderRule = do
    x <- many1 digit
    string "|"
    y <- many1 digit
    return $ PageOrderRule (read x) (read y)

parseUpdate :: Parser Update
parseUpdate = do
        n <- sepBy1 (many1 digit) (char ',')
        return $ Update $ map read n

readData :: IO ([String], [String])
readData = (\ls -> (head ls, head $ tail ls)) . groupBetweenBlankLines . lines <$> readFile "resource/2024/day05"

parseData :: ([String], [String]) -> ([PageOrderRule], [Update])
parseData (pos, us) = (rules, updates)
    where
        rules = map (fromRight' . parse parsePageOrderRule "") pos
        updates = map (fromRight' . parse parseUpdate "") us
------------------

reducePageOrderRules :: [PageOrderRule] -> Update -> [PageOrderRule]
reducePageOrderRules por (Update us) = filter (\(PageOrderRule a b) -> a `elem` us && b `elem` us) por

findPrecursors :: [PageOrderRule] -> Int -> [Int]
findPrecursors pors b = map (\(PageOrderRule l m) -> l) $ filter (\(PageOrderRule l m) -> m == b) pors

allPrecursorsExist :: [PageOrderRule] -> Int -> [Int] -> Bool
allPrecursorsExist pors n ns = Set.isSubsetOf ps nss
    where
        nss = Set.fromList ns
        ps = Set.fromList $ findPrecursors pors n

validateUpdate :: [PageOrderRule] -> Update -> Bool
validateUpdate pors u@(Update us) = validate reducedpors us
    where
        reducedpors  = reducePageOrderRules pors u
        validate ps []  =  True
        validate ps ur  =  if (allPrecursorsExist ps (last ur) (init ur)) then validate ps (init ur) else False

middleUpdate :: Update -> Int
middleUpdate (Update us) = head $ drop ((length us - 1) `div` 2) us

runPt1 = sum . map middleUpdate . (\(pors, us) -> filter (validateUpdate pors) us) . parseData <$> readData

--------------------

pageOrdering :: [PageOrderRule] -> Int -> Int -> Ordering
pageOrdering pors a b | a == b = EQ 
                      |(PageOrderRule a b) `elem` pors = LT
                      | otherwise = GT

reorderPages :: [PageOrderRule] -> Update -> Update
reorderPages pors (Update us) = Update (sortBy (pageOrdering pors) us)

runPt2 = sum . map middleUpdate .  (\(pors, us) -> map (reorderPages pors) $ filter (not . validateUpdate pors) us) . parseData <$> readData