module Day11_2020
    (
        day11Pt1,
        day11Pt2
    ) where

    import ListUtils
    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List (find)
    import Data.Maybe (fromJust, isJust, listToMaybe, catMaybes)
    import Data.Bifunctor (second)
    import qualified Data.HashMap.Strict as H

    data Space = Chair | Filled | Floor deriving (Eq, Show)

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

    neighbours = [(x,y) | x <-[-1,0,1], y <-[-1,0,1], (x,y) /= (0,0) ]


    countNeighboursOfType :: Position -> Space -> H.HashMap Position Space -> Int
    countNeighboursOfType (Position xp yp) space grid =  (length . filter (== space)) (catMaybes neighbourSpaces)
            where
          neighbourSpaces = map (\(nX, nY) -> H.lookup (Position (xp + nX) (yp + nY)) grid) neighbours


    evolvePosition :: (Position -> Space -> H.HashMap Position Space -> Int) -> Position -> Space -> H.HashMap Position Space -> Space
    evolvePosition f p Chair grid | f p Filled grid == 0 = Filled
                                | otherwise = Chair
    evolvePosition f p Filled grid | f p Filled grid >= 4 = Chair
                                 | otherwise = Filled
    evolvePosition _ _ Floor grid = Floor
    
    evolveModel :: H.HashMap Position Space -> H.HashMap Position Space
    evolveModel grid = H.mapWithKey (\p s -> evolvePosition countNeighboursOfType p s grid) grid


    loopUntilStable :: (H.HashMap Position Space -> H.HashMap Position Space) -> H.HashMap Position Space -> H.HashMap Position Space
    loopUntilStable evaluate grid | evaluate grid == grid = grid
                                  | otherwise = loopUntilStable evaluate k
                         where
                             k = evaluate grid

    countFilledChairs grid = length $ H.filter (== Filled) grid

    day11Pt1 = countFilledChairs . loopUntilStable evolveModel <$> setupGrid

    ----------

    searchInDirection :: Position -> (Int, Int) -> H.HashMap Position Space -> Maybe (Position, Space)
    searchInDirection (Position px py) (x, y) grid = listToMaybe $ map (second fromJust) $ filter (\(p,s) -> isJust s &&  (s == Just Filled || s == Just Chair)) $ map (\k -> (Position (px + k*x) (py + k*y), H.lookup (Position (px + k*x) (py + k*y)) grid)) [1..kMax]
        where
            xMax = maximum $ map (\(Position x _) -> x ) $ H.keys grid
            yMax = maximum $ map (\(Position _ y) -> y ) $ H.keys grid
            kMax | y == 0 = max ((xMax - px )`div` x) (- px `div` x) 
                 | x == 0 = max ((yMax -py) `div` y) (- py `div` y) 
                 | otherwise = max (max ((xMax - px )`div` x) (- px `div` x)) (max ((yMax -py) `div` y) (- py `div` y))


    countVisibleNeighboursOfType :: Position -> Space -> H.HashMap Position Space -> Int
    countVisibleNeighboursOfType (Position xp yp) space grid =  length $ filter (\(_, s)-> s == space) $ catMaybes neighbourSpaces
        where
          neighbourSpaces = map (\(nx, ny) ->  searchInDirection (Position xp yp) (nx, ny) grid) neighbours

    evolveNewModelPosition :: (Position -> Space -> H.HashMap Position Space -> Int) -> Position -> Space -> H.HashMap Position Space -> Space
    evolveNewModelPosition f p Chair grid | f p Filled grid == 0 = Filled
                                | otherwise = Chair
    evolveNewModelPosition f p Filled grid | f p Filled grid >= 5 = Chair
                                 | otherwise = Filled
    evolveNewModelPosition f p Floor grid = Floor
    
    evolveNewModel :: H.HashMap Position Space -> H.HashMap Position Space
    evolveNewModel grid = H.mapWithKey (\p s -> evolveNewModelPosition countVisibleNeighboursOfType p s grid) grid

    day11Pt2 = countFilledChairs . loopUntilStable evolveNewModel <$> setupGrid

    ---------

    setupGrid :: IO (H.HashMap Position Space)
    setupGrid = H.fromList . map ((\x -> (fst x, fromRight' $ parse parseSpace "" (snd x))) . (\x -> (fst x, [snd x]))) . convertToPositionList <$> readLayout

    readLayout :: IO [String]
    readLayout = lines <$> readFile "resource/2020/day11"

    {--
    PT1
    2386

    real	2m33.836s
    user	2m34.161s
    sys	    0m0.230s

    with hashmap
    2386

    real	0m1.458s
    user	0m1.829s
    sys	0m0.534s

    PT2 with hashmap
    2091

    real	10m0.148s
    user	13m22.772s
    sys	1m22.937s
    --}