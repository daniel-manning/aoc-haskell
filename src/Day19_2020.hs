module Day19_2020
    (
      day19Pt1
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import ListUtils (groupBetweenBlankLines)
    import Control.Monad (join)
    import Data.List (find, foldl', sort, nub)
    import Data.Maybe (fromJust)
    import Data.Universe.Helpers (choices)
    import qualified Data.HashMap.Strict as H

    data Rule = S [String] | R [[Int]] deriving Show

    parseRuleC :: Parser [Int]
    parseRuleC = do
            e <- many1 (many1 digit <* spaces)
            return $ map read e

    parseOrRule :: Parser Rule
    parseOrRule = do
        m <- parseRuleC
        string "| "
        n <- parseRuleC
        return $ R [m,n]

    parseSingleRule = do
        r <- parseRuleC
        return $ R [r]

    parseString :: Parser Rule
    parseString = do
        char '"'
        i <- many1 letter
        char '"'
        return $ S [i]

    parseIndexedRule :: Parser (Int, Rule)
    parseIndexedRule = do
         i <- many1 digit
         string ": "
         r <- choice [try parseOrRule, try parseSingleRule, try parseString]
         return (read i, r)
    -------------
    runRules :: H.HashMap Int Rule -> H.HashMap Int Rule -> H.HashMap Int Rule
    runRules ns [] = ns
    runRules ns ((e,S rs):xs) = runRules ((e,S rs): ns) xs
    runRules ns ((e, R rs):xs) | null [ x | x@(R _) <- map snd $ filter (\(i, r) -> i `elem` join rs ) (xs ++ ns)] = runRules (newStrings : ns) xs
                                 | otherwise = runRules ((e, R rs): ns) xs -- not ready to play
        where
          newStrings = (e, expandRules rs (filter (\(i, r) -> i `elem` join rs) (xs ++ ns)))

    finishedRules :: H.HashMap Int Rule -> H.HashMap Int Rule
    finishedRules xs | null [ x | x@(R _) <- map snd expanded] = expanded
                     | otherwise = finishedRules expanded
       where expanded = runRules [] xs

    --test = [(0, R [[4,1,5]]),(1, R [[2,3],[3,2]]),(2, S ["aa", "ab"]), (3, S ["ab", "ba"]),(4, S ["a"]),(5, S ["b"])]


    expandRules :: [[Int]] -> H.HashMap Int Rule -> Rule
    expandRules rs refs = S $ sort $ nub $ join $ map (\x -> map (foldl' (++) "") $ choices $ map (\y ->  (\(S s) -> s) $ snd $ fromJust $ find (\(i,_) -> i == y) refs) x) rs

    
    day19Pt1 = (\x -> length $ filter(\l -> l `elem` validMessages x) $ snd x) <$> readRulesAndMessages
       where
         validMessages x = ((\(S s) -> s) . snd . fromJust . find (\(i,_) -> i == 0) . finishedRules . fst) x

    --day19Pt1 = (\(S s) -> s) . snd . fromJust . find (\(i,_) -> i == 0) . finishedRules . fst <$> readRulesAndMessages
       
    
    readRulesAndMessages :: IO (H.HashMap Int Rule, [String])
    readRulesAndMessages = (\x -> (H.fromList $ map (fromRight' . parse parseIndexedRule "") $ head x, x !! 1)) . groupBetweenBlankLines . lines <$> readFile "resource/2020/day19"
