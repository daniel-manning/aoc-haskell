module Day18_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import ListUtils (groupBetweenBlankLines)

    data Rule = S [String] | R [[Int]] deriving Show

    parseRuleC :: Parser [Int]
    parseRuleC = do
            e <- many1 (many1 digit <* spaces)
            return $ map read $ e

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

    readRulesAndMessages :: IO ([(Int, Rule)], [String])
    readRulesAndMessages = (\x -> (map (fromRight' . parse parseIndexedRule "") $ head x, x !! 1)) . groupBetweenBlankLines . lines <$> readFile "resource/2020/day19_test"
