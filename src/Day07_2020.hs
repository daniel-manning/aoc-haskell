module Day04_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.Char (digitToInt)
    import Data.List (find)
    import Data.Maybe

    data BagRule = BagRule String [(Int, String)] deriving Show

    parseBagColour :: Parser String
    parseBagColour = do
        colour <- many1 letter
        space
        colour2 <- many1 letter
        space
        choice [try (string "bags"), try (string "bag")]
        return (colour ++ " " ++ colour2)


    parseNoBags :: Parser [(Int, String)]
    parseNoBags = do
        string "no other bags"
        return []

    parseBagList :: Parser [(Int, String)]
    parseBagList = do
        bags <- sepBy parseNumAndBagColour (char ',' *> space)
        return bags

    parseNumAndBagColour :: Parser (Int, String)
    parseNumAndBagColour = do
        num <- digit
        space
        colour <- parseBagColour
        return (digitToInt num, colour)

    parseBagRule = do
        bag <- parseBagColour
        space
        string "contain"
        space
        contentsList <- choice [parseNoBags, try parseBagList]
        char '.'
        return $ BagRule bag contentsList

    runRules =  length . filter (\l -> "shiny gold" `elem` l) . findAllSubBags . map (fromRight' . parse parseBagRule "") <$> readRules

    filterDown :: [BagRule] -> BagRule ->  [String]
    filterDown _ (BagRule colour [] ) = []
    filterDown bagRules (BagRule colour innerBags) = (map snd innerBags) ++ ((\b -> filterDown bagRules $ fromJust $ find (\(BagRule c ib) -> c == (snd b)) bagRules ) =<< innerBags)



    findAllSubBags rules = map (filterDown rules) rules

    readRules :: IO [String]
    readRules = lines <$> readFile "resource/2020/day07"



