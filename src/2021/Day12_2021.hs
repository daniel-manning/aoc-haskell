module Day12_2021 where

import qualified Data.Map.Strict as Map
import Data.List (sortBy, groupBy, sort, group)
import Data.Char ( isLower ) 
import Data.List.Extra (snoc)
import Data.Function (on)
import Data.Maybe (fromJust)

data CaveLink = CaveLink Name Name deriving Show
newtype Name = Name String deriving (Eq, Ord, Show)

-- | Split is like break, but the matching element is dropped.
split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left,right)
        where
        (left,right')=break f s
        right = if null right' then [] else tail right'

-- | Repeadly splits a list by the provided separator and collects the results
splitList :: Eq a => a -> [a] -> [[a]]
splitList _   [] = []
splitList sep list = h:splitList sep t
        where (h,t)=split (==sep) list

parseCaveLink :: String -> CaveLink
parseCaveLink s = CaveLink (Name $ head a) (Name $ head $ tail a)
    where
        a = splitList '-' s

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day12"

readAndParse :: IO [CaveLink]
readAndParse = map parseCaveLink <$> readData
------------
constructMap :: [CaveLink] -> Map.Map Name [Name]
constructMap cls = Map.fromList $ map (\xs -> (fst $ head xs, map snd xs)) $ groupBy (\a b -> fst a == fst b) $ sortBy (compare `on` fst) $ (\(CaveLink a b) -> [(a, b), (b,a)]) =<< cls


removeBasedOnPrevious :: [Name] -> Name -> Bool
removeBasedOnPrevious previous n = not (n `elem` previous && all isLower ((\(Name x) -> x) n))

searchNextStep :: ([Name] -> Name -> Bool) -> [Name] -> Name -> Map.Map Name [Name] -> [([Name], Name)]
searchNextStep p previous s steps = map (\n -> (previous `snoc` n, n)) allowed
        where
            next = fromJust $ Map.lookup s steps
            allowed = filter (p previous) next

continueUntilHitNode :: ([Name] -> Name -> Bool) -> Name -> [[Name]] ->  [([Name], Name)] -> Map.Map Name [Name] -> [[Name]]
continueUntilHitNode _ _ finished [] _ = finished
continueUntilHitNode p stop finished xs steps = continueUntilHitNode p stop (finished ++ nowFinished) walkOneStep steps
    where
        nowFinished = map fst $ filter (\x -> snd x == stop) xs
        going =  filter (\x -> snd x /= stop || length x < 10) xs
        walkOneStep = (\x -> uncurry (searchNextStep p) x steps) =<< going


runWalk :: ([Name] -> Name -> Bool) -> Name -> Map.Map Name [Name] -> [[Name]]
runWalk p stop = continueUntilHitNode p stop [] [([Name "start"], Name "start")]


runPt1 :: Map.Map Name [Name] -> Int
runPt1 = length . runWalk removeBasedOnPrevious (Name "end")
------------------------------
removeManyVisitsToCaves :: [Name] -> Name -> Bool
removeManyVisitsToCaves previous n =
       not (n `elem` previous && lowN) || (n /= Name "start"  && n /= Name "end" && not haveAlreadyDoubleVisited)
    where
        lowN = all isLower ((\(Name x) -> x) n)
        haveAlreadyDoubleVisited = any (\x -> length x > 1) $ group $ sort $ filter (all isLower . (\(Name x) -> x)) previous

runPt2 :: Map.Map Name [Name] -> Int
runPt2 = length . runWalk removeManyVisitsToCaves (Name "end")


solution :: IO Int
solution = runPt2 . constructMap <$> readAndParse