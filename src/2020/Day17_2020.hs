{-# LANGUAGE TupleSections #-}

module Day17_2020
    (
    day17Pt1
    ) where

    import qualified Data.HashMap.Strict as H
    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.Maybe (fromJust, isJust, listToMaybe, catMaybes, isNothing)
    import Data.Hashable
    import Data.List (nub)

    data Space = Active | Inactive deriving (Eq, Show)
    data Position = Position Int Int Int deriving (Eq, Show)
    instance Hashable Position where
            hashWithSalt s (Position x y z) =
                s `hashWithSalt`
                x `hashWithSalt`
                y `hashWithSalt` z

    parseActive = do
            char '#'
            return Active

    parseInactive = do
               char '.'
               return Inactive

    parseSpace = choice [try parseActive, try parseInactive]


    neighbours = [(x,y,z) | x <-[-1,0,1], y <-[-1,0,1], z <-[-1,0,1], (x,y,z) /= (0,0,0) ]

    countNeighboursOfType :: Position -> Space -> H.HashMap Position Space -> Int
    countNeighboursOfType (Position xp yp zp) space grid =  (length . filter (== space)) (catMaybes neighbourSpaces)
            where
          neighbourSpaces = map (\(nX, nY, nZ) -> H.lookup (Position (xp + nX) (yp + nY) (zp + nZ)) grid) neighbours


    evolvePosition :: (Position -> Space -> H.HashMap Position Space -> Int) -> Position -> Space -> H.HashMap Position Space -> Space
    evolvePosition f p Active grid | f p Active grid == 2 || f p Active grid == 3 = Active
                                   | otherwise = Inactive
    evolvePosition f p Inactive grid | f p Active grid == 3 = Active
                                     | otherwise = Inactive

    evolveModel :: H.HashMap Position Space -> H.HashMap Position Space
    --needs an earlier stage to wrap each active cell with a neighbourhood of inactive ones
    inactiveNeighbourhoodAboutPoint :: H.HashMap Position Space -> Position -> [Position]
    inactiveNeighbourhoodAboutPoint grid (Position x y z) = map fst $ filter (\(p, m) -> isNothing m) neighbourSpaces
        where
          neighbourSpaces = map (\(nX, nY, nZ) -> (Position (x + nX) (y + nY) (z + nZ), H.lookup (Position (x + nX) (y + nY) (z + nZ)) grid)) neighbours

    inactiveLayer grid = H.fromList $ map (, Inactive) .  nub . inactiveNeighbourhoodAboutPoint grid =<< H.keys (H.filter (== Active) grid)
    ----
    evolveModel grid = H.mapWithKey (\p s -> evolvePosition countNeighboursOfType p s grid) (grid `H.union` inactiveLayer grid)

    convertToPositionList :: [[a]] -> [(Position, a)]
    convertToPositionList list = (\l -> zipWith (curry (\ x -> (Position (fst x) (fst l) 0, snd x))) [0..]  (snd l))  =<< zip [0..] list

    countActiveSpaces grid = length $ H.filter (== Active) grid

    times :: Int -> (a -> a) -> a -> a
    times 0 _ k = k
    times n f k = times (n-1) f $! f k

    day17Pt1 = countActiveSpaces . times 6 evolveModel <$> setupGrid

    setupGrid :: IO (H.HashMap Position Space)
    setupGrid = H.fromList . map ((\x -> (fst x, fromRight' $ parse parseSpace "" (snd x))) . (\x -> (fst x, [snd x]))) . convertToPositionList <$> readLayout

    readLayout :: IO [String]
    readLayout = lines <$> readFile "resource/2020/day17"

    {--
    380

    real	0m1.584s
    user	0m1.592s
    sys	0m0.172s
    --}