module Day24_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List (foldl', sort, group)
    
    data HexTile = HexTile Int Int Int deriving (Ord, Eq, Show)
    data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving Show
    data TileSide = Black | White deriving Show

    referenceTile = HexTile 0 0 0

    move :: Direction -> HexTile -> HexTile
    move East       (HexTile x y z) = HexTile  (x+1) (y-1)   z
    move SouthEast  (HexTile x y z) = HexTile    x   (y-1) (z+1)
    move SouthWest  (HexTile x y z) = HexTile  (x-1)   y   (z+1)
    move West       (HexTile x y z) = HexTile  (x-1) (y+1)   z 
    move NorthWest  (HexTile x y z) = HexTile    x   (y+1) (z-1)
    move NorthEast  (HexTile x y z) = HexTile  (x+1)   y   (z-1)

    parseNorthWest = do
        string "nw"
        return NorthWest

    parseNorthEast = do
        string "ne"
        return NorthEast

    parseSouthWest = do
        string "sw"
        return SouthWest

    parseSouthEast = do
        string "se"
        return SouthEast
    
    parseEast = do
        string "e"
        return East
    
    parseWest = do
        string "w"
        return West

    parseSteps = do many1 (choice [try parseNorthWest, try parseNorthEast, try parseSouthWest, try parseSouthEast, try parseEast, try parseWest])

    filterFlipBacks = filter (\(a,b) -> not $ even b)

    makeFrequencyList = map (\x -> (head x, length x)) . group . sort 

    tilesToFlip = length . filterFlipBacks . makeFrequencyList . map (foldl' (flip move) referenceTile . fromRight' . parse parseSteps "") <$> instructions

    instructions :: IO [String]
    instructions = lines <$> readFile "resource/2020/day24"

