module Day11_2020
    (
    ) where

    import ListUtils (convertToPositionList)

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators

    data Space = Chair | Filled | Floor deriving Show

    parseChair =  do
        char 'L'
        return Chair

    parseFilled = do
        char '#'
        return Filled

    parseFloor = do
        char '.'
        return Floor

    parseSpace :: Parser Space
    parseSpace = choice [try parseChair, try parseFilled, try parseFloor]

    test =  convertToPositionList <$> readLayout

    readLayout :: IO [String]
    readLayout = lines <$> readFile "resource/2020/day11_test"