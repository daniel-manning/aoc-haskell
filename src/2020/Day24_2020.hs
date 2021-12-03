{-# LANGUAGE TupleSections #-}
module Day24_2020
    (
        day24Pt2
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List (foldl', sort, group, nub)
    import Data.Maybe (fromMaybe, isJust, isNothing)
    import Data.Hashable
    import qualified Data.HashMap.Strict as H
    
    
    data HexTile = HexTile Int Int Int deriving (Ord, Eq, Show)
    instance Hashable HexTile where
            hashWithSalt s (HexTile x y z) =
                s `hashWithSalt`
                x `hashWithSalt`
                y `hashWithSalt` z


    data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving Show
    data TileSide = Black | White deriving (Eq, Show)

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

    day24Pt1 = length . filterFlipBacks . makeFrequencyList . map (foldl' (flip move) referenceTile . fromRight' . parse parseSteps "") <$> instructions

    -----------------
    neighbourhood :: HexTile -> [HexTile]
    neighbourhood tile = map (`move` tile) [East, SouthEast, SouthWest, West, NorthWest, NorthEast]

    evolveTile :: HexTile -> TileSide -> H.HashMap HexTile TileSide -> TileSide
    evolveTile t Black grid | adjacentBlackTiles == 1 || adjacentBlackTiles == 2 = Black
                            | otherwise = White
        where 
            adjacentBlackTiles =  length $ filter (== Black) $ map (\tile -> fromMaybe White $ H.lookup tile grid) $ neighbourhood t 
    evolveTile t White grid | adjacentBlackTiles == 2 = Black
                            | otherwise = White
        where 
            adjacentBlackTiles =  length $ filter (== Black) $ map (\tile -> fromMaybe White $ H.lookup tile grid) $ neighbourhood t 

    evolveFloor ::  H.HashMap HexTile TileSide -> H.HashMap HexTile TileSide
    evolveFloor floor = H.mapWithKey (\k v -> evolveTile k v tilesWithPadding) tilesWithPadding
        where
            tilesWithPadding = floor `H.union` padding floor

    padding :: H.HashMap HexTile TileSide -> H.HashMap HexTile TileSide
    padding grid =  H.fromList $ map (, White) . filter (\t -> isNothing $ H.lookup t grid) . nub . neighbourhood =<< H.keys (H.filter (== Black) grid)

    countBlackTiles = H.size . H.filter (== Black)

    times :: Int -> (a -> a) -> a -> a
    times 0 _ k = k
    times n f k = times (n-1) f $! f k

    day24Pt2 = countBlackTiles . times 100 evolveFloor <$> tilePlan
    
    tilePlan =  H.fromList . map ((, Black) . fst) . filterFlipBacks . makeFrequencyList .  map (foldl' (flip move) referenceTile . fromRight' . parse parseSteps "") <$> instructions

    -----------------
    instructions :: IO [String]
    instructions = lines <$> readFile "resource/2020/day24"

    {--
    3565

    real	0m1.102s
    user	0m1.337s
    sys	0m0.315s

    --}

