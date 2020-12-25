module Day19_2020
    (
      day19Pt1
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import ListUtils (groupBetweenBlankLines)
    import Control.Monad (join)
    import Data.List (find, foldl', sort, nub)
    import Data.Maybe (fromJust, isJust)
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
    runRules :: H.HashMap Int Rule -> H.HashMap Int Rule
    runRules rulemap = foldl' (\b (i,s) -> H.insert i s b) rulemap $ H.toList expanded
        where
          expanded = H.map newStrings $ H.filter (\(R rs) -> all (\i -> isJust $ H.lookup i onlyStrings) (join rs)) onlyRules
          onlyRules = H.filterWithKey (\k v -> case v of (R _) -> True; _ -> False ) rulemap
          onlyStrings = H.filterWithKey (\k v -> case v of (S _) -> True; _ -> False ) rulemap
          newStrings (R rs) = expandRules rs (H.filterWithKey (\i r -> i `elem` join rs) rulemap)
    

    finishedRules :: H.HashMap Int Rule -> H.HashMap Int Rule
    finishedRules xs | null [ x | x@(R _) <- H.elems xs] = expanded
                     | otherwise = finishedRules expanded
       where expanded = runRules xs

    expandRules :: [[Int]] -> H.HashMap Int Rule -> Rule
    expandRules rs refs = S $ sort $ nub $ join $ map (map (foldl' (++) "") . choices . map (\y ->  (\(S s) -> s) $ fromJust $ H.lookup y refs)) rs

    
    {--day19Pt1 = (\x -> length $ filter(\l -> l `elem` validMessages x) $ snd x) <$> readRulesAndMessages
       where
         validMessages x = ((\(S s) -> s) . fromJust . H.lookup 0 . finishedRules . fst) x --}

    day19Pt1 = finishedRules . fst <$> readRulesAndMessages
       
    
    readRulesAndMessages :: IO (H.HashMap Int Rule, [String])
    readRulesAndMessages = (\x -> (H.fromList $ map (fromRight' . parse parseIndexedRule "") $ head x, x !! 1)) . groupBetweenBlankLines . lines <$> readFile "resource/2020/day19_test"
