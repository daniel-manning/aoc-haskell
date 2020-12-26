module Day19_2020
    (
      day19Pt1
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import ListUtils (groupBetweenBlankLines)
    import Control.Monad (join, foldM)
    import Data.List (find, foldl', sort, nub)
    import Data.Maybe (fromJust, isJust, fromMaybe)
    import Data.Universe.Helpers (choices)
    import qualified Data.HashMap.Strict as H
    import Data.String.Utils (split, splitWs, startswith, endswith)

    data Rule = S String | R [[Int]] deriving Show

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
        return $ S i

    parseIndexedRule :: Parser (Int, Rule)
    parseIndexedRule = do
         i <- many1 digit
         string ": "
         r <- choice [try parseOrRule, try parseSingleRule, try parseString]
         return (read i, r)
    -------------
    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse (Just x) _ = Just x
    orElse Nothing  x = x
    
    matchRule :: Rule -> String -> H.HashMap Int Rule -> Maybe String
    matchRule (R [ra,rb]) xs ruleMap = orElse (runRule ra) (runRule rb)
        where
          lookUpAndMatch i xs ruleMap = matchRule (fromJust $ H.lookup i ruleMap) xs ruleMap
          runRule x = foldM (\a b -> lookUpAndMatch b a ruleMap) xs x
    matchRule (R [r]) xs ruleMap = foldM (\a b -> lookUpAndMatch b a ruleMap) xs r
        where
          lookUpAndMatch i xs ruleMap = matchRule (fromJust $ H.lookup i ruleMap) xs ruleMap
    matchRule (S a) xs _ | a `startswith` xs = Just (drop (length a) xs)
                         | otherwise = Nothing

    matchFormatRule :: H.HashMap Int Rule -> String -> Maybe String
    matchFormatRule ruleMap xs = matchRule (fromJust $ H.lookup 0 ruleMap) xs ruleMap

    day19Pt1 = length . filter (== Just "") . (\(a,b) -> map (\x -> matchFormatRule a x) b) <$> readRulesAndMessages
       
    
    readRulesAndMessages :: IO (H.HashMap Int Rule, [String])
    readRulesAndMessages = (\x -> (H.fromList $ map (fromRight' . parse parseIndexedRule "") $ head x, x !! 1)) . groupBetweenBlankLines . lines <$> readFile "resource/2020/day19"
