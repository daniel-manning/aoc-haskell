module Day07_2017 (
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators (fromRight', mapRight, fromLeft')
import Data.Either (isLeft)
import Data.Bifunctor (second)
import Data.List (foldl', group, sort, groupBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Control.Applicative (liftA2)
import Control.Monad (sequence)
import Data.Maybe
import Debug.Trace

data Tower = Tower {
    name :: String,
    weight :: Maybe Int,
    discs :: [Tower]
} deriving (Show, Eq)

emptyTower n = Tower n Nothing []


parseTower :: Parser Tower
parseTower = do
    n <- many1 letter
    string " ("
    w <- many1 digit
    string ")"
    ds <- option [] parseDiscs
    return $ Tower n (Just $ read w) ds

parseDiscs :: Parser [Tower]
parseDiscs = do
    string " -> "
    ds <- sepBy1 (many1 letter) (string ", ")
    return $ map emptyTower ds


readData :: IO [String]
readData = lines <$> readFile "resource/2017/day07"

readAndParse :: IO [Tower]
readAndParse = map (fromRight' . parse parseTower "") <$> readData
-----------------

branches  = filter (\(Tower _ _ ds) -> not (null ds))

rootOfBranches = map name . branches
allTowers = map name

followingNodes = concatMap ( map name . discs) . branches

rootTowerName ts = head $ Set.toList $ Set.fromList (allTowers ts) `Set.difference` Set.fromList (followingNodes ts)

runPt1 = rootTowerName <$> readAndParse
--------------
rootTower :: [Tower] -> Tower
rootTower ts = head $ filter (\(Tower ns _ _) -> ns == n) ts
    where
        n = rootTowerName ts

insert (Tower n w ds) (Tower n' w' ds') | n == n' = Tower n' w' ds'
                                        | otherwise = Tower n w (map (\t -> insert t (Tower n' w' ds')) ds)

--insert requires a list of names of nodes in the right order to build up from root
nodeNames :: [Tower] -> [String]
nodeNames ts = nextNames ts (rootTower ts)

findTowerByName :: [Tower] -> String -> Tower
findTowerByName ts n = head $ filter (\(Tower ns _ _) -> ns == n) ts

nextNames :: [Tower] -> Tower -> [String]
nextNames ts (Tower n _ ds) = n : concatMap (nextNames ts . findTowerByName ts . name) ds 

--create tree by folding up root with list of children
buildUpTree :: [Tower] -> Tower
buildUpTree ts = foldl' (\t n -> insert t (findTowerByName ts n)) (rootTower ts) (nodeNames ts)

allEqual :: (Ord a) => [a] -> Bool
allEqual as = length (group (sort as)) == 1

findUniqueElem :: (Ord a, Ord b) => [(a,b)] -> (a,b)
findUniqueElem = head . head . filter (\ts -> length ts == 1) . groupBy (\a b -> fst a == fst b) . sort

commonWeight :: (Ord a, Ord b) => [(a,b)] -> a
commonWeight =  fst. head . head . filter (\ts -> length ts /= 1) . groupBy (\a b -> fst a == fst b) . sort

allWeightsEqual :: Either a [Int] -> Bool
allWeightsEqual (Right ws) = allEqual ws
allWeightsEqual _ = False

checkWeights :: Tower -> Either (Int, String) Int
checkWeights (Tower n w ds) | null ds = Right (fromJust w)
                            | allWeightsEqual ws = mapRight (\ts -> fromJust w + sum ts) ws
                            | isLeft ws = Left $ fromLeft' ws
                            | otherwise = Left $ (\vs -> (weightDiff vs, elem vs)) $ zipWith (curry (second name)) (fromRight' ws) ds
    where
        ws = mapM checkWeights ds
        elem vs = snd $ findUniqueElem vs
        weightDiff vs = commonWeight vs - fst (findUniqueElem vs)


findDiscWeightByName :: Tower -> String -> [Maybe Int]
findDiscWeightByName t n | name t == n = [weight t]
                         | null (discs t) = [Nothing]
                         | otherwise = filter (/= Nothing) (flip findDiscWeightByName n =<< discs t)

elementWeightShouldBe :: Tower -> (Int, String) -> Int
elementWeightShouldBe t (wd, n) = wd + fromJust (head $ findDiscWeightByName t n)

runPt2 = (\t -> elementWeightShouldBe t $ fromLeft' $ checkWeights t) . buildUpTree <$> readAndParse
