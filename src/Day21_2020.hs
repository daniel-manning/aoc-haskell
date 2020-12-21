module Day21_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators

    data Food = Food [String] [String] deriving Show

    parseWord :: Parser String
    parseWord = do
        spaces
        w <- many1 letter
        spaces
        return w

    parseFood :: Parser Food
    parseFood = do
        ingredients <- many1 parseWord
        string "(contains"
        allergies <- sepBy1 parseWord (char ',')
        char ')'
        return $ Food ingredients allergies


    findFoodLists = map (fromRight' . parse parseFood "") <$> inputAllergyList
    
    inputAllergyList :: IO [String]
    inputAllergyList = lines <$> readFile "resource/2020/day21_test"
        