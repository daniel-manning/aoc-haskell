module Day11_2020
    (
        day11Pt1,
        day11Pt2
    ) where

    import ListUtils
    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List (find)
    import Data.Maybe (fromJust, isJust, listToMaybe)

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


    countNeighboursOfType :: Position -> Space -> [(Position, Space)] -> Int
    countNeighboursOfType (Position xp yp) space grid =  length . filter (\(_, s)-> s == space) . map fromJust $ filter isJust neighbourSpaces
        where
          neighbourSpaces = map (\(nX, nY) -> find (\(Position x y, _) -> (xp + nX == x) && (yp + nY == y)) grid) neighbours


    evolvePosition :: (Position -> Space -> [(Position, Space)] -> Int) -> Position -> Space -> [(Position, Space)] -> (Position, Space)
    evolvePosition f p Chair grid | (f p Filled grid) == 0 = (p, Filled)
                                | otherwise = (p, Chair)
    evolvePosition f p Filled grid | f p Filled grid >= 4 = (p, Chair)
                                 | otherwise = (p, Filled)
    evolvePosition f p Floor grid = (p, Floor)
    
    evolveModel :: [(Position, Space)] -> [(Position, Space)]
    evolveModel grid = map (\(p, s) -> evolvePosition countNeighboursOfType p s grid) grid


    loopUntilStable :: ([(Position, Space)] -> [(Position, Space)]) -> [(Position, Space)] -> [[(Position, Space)]]
    loopUntilStable evaluate grid | evaluate grid == grid = []
                           | otherwise = k :  loopUntilStable evaluate k
                         where
                             k = evaluate grid

    countFilledChairs grid = length $ filter (\(_, s) -> s == Filled) grid

    day11Pt1 = countFilledChairs . last . (loopUntilStable evolveModel) <$> setupGrid

    ----------

    searchInDirection :: Position -> (Int, Int) -> [(Position, Space)] -> Maybe (Position, Space)
    searchInDirection (Position px py) (x, y) grid =  id =<< (listToMaybe $ filter isJust $ map (\k -> find (\(p, s) -> (p == Position (px + k*x) (py + k*y)) && (s == Filled || s == Chair)) grid) [1..kMax])
        where
            xMax = maximum $ map (\(Position x _, _) -> x ) grid
            yMax = maximum $ map (\(Position _ y, _) -> y ) grid
            kMax = if (y == 0) then max ((xMax - px )`div` x) (- px `div` x) else if (x == 0) then max ((yMax -py) `div` y) (- py `div` y) else max (max ((xMax - px )`div` x) (- px `div` x)) (max ((yMax -py) `div` y) (- py `div` y))


    countVisibleNeighboursOfType :: Position -> Space -> [(Position, Space)] -> Int
    countVisibleNeighboursOfType (Position xp yp) space grid =  length $ filter (\(_, s)-> s == space) $ map fromJust $ filter isJust neighbourSpaces
        where
          neighbourSpaces = map (\(nx, ny) ->  searchInDirection (Position xp yp) (nx, ny) grid) neighbours

    evolveNewModelPosition :: (Position -> Space -> [(Position, Space)] -> Int) -> Position -> Space -> [(Position, Space)] -> (Position, Space)
    evolveNewModelPosition f p Chair grid | (f p Filled grid) == 0 = (p, Filled)
                                | otherwise = (p, Chair)
    evolveNewModelPosition f p Filled grid | f p Filled grid >= 5 = (p, Chair)
                                 | otherwise = (p, Filled)
    evolveNewModelPosition f p Floor grid = (p, Floor)
    
    evolveNewModel :: [(Position, Space)] -> [(Position, Space)]
    evolveNewModel grid = map (\(p, s) -> evolveNewModelPosition countVisibleNeighboursOfType p s grid) grid

    day11Pt2 = countFilledChairs . last . (loopUntilStable evolveNewModel) <$> setupGrid

    setupGrid :: IO [(Position, Space)]
    setupGrid = map (\x -> (fst x, (fromRight' $ (parse parseSpace "" (snd x))))) . map (\x -> (fst x, [snd x])) . convertToPositionList <$> readLayout

    readLayout :: IO [String]
    readLayout = lines <$> readFile "resource/2020/day11"

    {--
    PT1
    2386

    real	2m33.836s
    user	2m34.161s
    sys	    0m0.230s

    --}