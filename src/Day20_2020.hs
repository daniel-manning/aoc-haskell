module Day20_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import ListUtils

    data Pixel = Black | White deriving Show

    parseBlack = do
        char '#'
        return Black

    parseWhite = do
       char '.'
       return White

    parsePixel = choice [try parseBlack, try parseWhite]



    --setupGrid :: IO (H.HashMap Position Space)
    setupGrid = map (\xs -> ((head xs), grid (tail xs))) <$> readLayout
        where
          --tileName = fromRight' $ parse parseSpace ""
          grid = map ((\x -> (fst x, fromRight' $ parse parsePixel "" (snd x))) . (\x -> (fst x, [snd x]))) . convertToPositionList


    --readLayout :: IO [String]
    readLayout = map reverse . groupBetweenBlankLines . lines <$> readFile "resource/2020/day20_test"