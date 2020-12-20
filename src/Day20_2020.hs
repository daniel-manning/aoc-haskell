module Day20_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import ListUtils
    import Data.List (transpose, intersect)

    data Pixel = Black | White deriving (Eq, Show)
    data Tile = Tile Integer [[Pixel]] [[Pixel]] deriving (Eq, Show)

    parseBlack :: Parser Pixel
    parseBlack = do
        char '#'
        return Black

    parseWhite :: Parser Pixel
    parseWhite = do
       char '.'
       return White

    parsePixels :: Parser [Pixel]
    parsePixels = do
        n <- many1 (choice [try parseBlack, try parseWhite])
        return n

    parseTileName :: Parser Integer
    parseTileName = do
        string "Tile "
        n <- many1 digit
        char ':'
        return $ read n

    day20Pt1 = product . findCorner . matchSides <$> setupGrid


    findCorner :: [(Integer, [Integer])] -> [Integer]
    findCorner = map fst . filter (\(i, xs) -> length xs == 2)

    --matchSides :: [Tile] -> [(Tile, [Tile])] -- This should me some kind of map but I can't be bothered
    matchSides :: [Tile] -> [(Integer, [Integer])]
    matchSides ts = map (\t@(Tile id _ sides) -> (id, map (\(Tile idT _ sidesT) -> idT) $ sharesSides t)) ts
        where
            sharesSides t@(Tile id _ sides) = filter (/= t) $ filter (\(Tile idT _ sidesT) -> not $ null (rotate sides `intersect` rotate sidesT)) ts
            rotate x = x ++ map reverse x
    
    setupGrid = map (\xs -> Tile (tileName (head xs)) (grid (tail xs)) (sides (grid (tail xs)))) <$> readLayout
        where
          tileName = fromRight' . parse parseTileName ""
          --grid = map ((\x -> (fst x, fromRight' $ parse parsePixel "" (snd x))) . (\x -> (fst x, [snd x]))) . convertToPositionList
          grid = map (fromRight' . parse parsePixels "")
          sides x = [head x, last x, head $ transpose x, last $ transpose x]


    --readLayout :: IO [String]
    readLayout = map reverse . groupBetweenBlankLines . lines <$> readFile "resource/2020/day20"