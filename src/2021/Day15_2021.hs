module Day15_2021 where
import Data.Maybe (catMaybes)
import Data.List (find, (\\), minimumBy)
import Data.Function ( on ) 

data Position  = Position Int Int deriving (Eq, Show)

split :: String -> [Int]
split = map (read . (:[]))

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day15"

readAndParse :: IO [[Int]]
readAndParse = map split <$> readData
----------------

maxPosition :: [[a]] -> Position
maxPosition xs = Position (length (head xs) - 1) (length xs - 1)

get :: Position -> [[a]] -> a
get (Position x y) xs = (xs !! y) !! x

nextStep :: Position -> [[Int]] -> (Position, Int) -> [(Position, Int)]
nextStep (Position maxX maxY) xs (Position x y, n) = catMaybes [down, along]
    where
        down = if y < maxY then Just (Position x (y+1), n + get (Position x (y+1)) xs) else Nothing
        along = if x < maxX then Just (Position (x+1) y, n + get (Position (x+1) y) xs) else Nothing

searchForEnd :: Position -> [[Int]] -> [(Position, Int)]  -> [(Position, Int)] -> [(Position, Int)]
searchForEnd _ _ [] fs = fs 
searchForEnd endPosition xs ps fs = searchForEnd endPosition xs (nextStep endPosition xs =<< going) (fs ++ finished)
    where
        finished = filter (\l -> fst l == endPosition) ps
        going = ps \\ finished

solution = snd . minimumBy (compare `on` snd) . (\l -> searchForEnd (maxPosition l) l [(Position 0 0, 0)] []) <$> readAndParse