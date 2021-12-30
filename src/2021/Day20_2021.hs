{-# LANGUAGE TupleSections #-}
module Day20_2021 (
    solution
)
 where

import Text.ParserCombinators.Parsec (many1, letter, Parser, parse, char, string, (<|>), digit)
import Data.Either.Combinators ( fromRight' )
import Data.String.Utils (rstrip)
import Data.Function (on)
import Data.List (sort, nub, minimumBy, maximumBy, find)
import Data.Maybe (fromMaybe, fromJust)
import ListUtils(Position, Position (..), convertToPositionList)
import Data.Set (Set, fromList, toList, empty, union)
import Data.List.Extra (notNull)

data Pixel = Light | Dark deriving (Eq, Show)

type Floor = [(Position, Pixel)]
type IEA = [Pixel]

data Mapping = Stable | BiStable | Other deriving Show

parsePixels:: Parser [Pixel]
parsePixels = do
    x1 <- many1 (char '.' <|> char '#')
    return $ map (\c -> if c == '.' then Dark else Light) x1


parseIEAFloor :: [String] -> (IEA, Floor)
parseIEAFloor xs = (fromRight' $ parse parsePixels "" (head xs), convertToPositionList $ map (fromRight' . parse parsePixels "") (takeWhile notNull $ drop 2 xs))

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day20"
-------------------

readAndParse :: IO (IEA, Floor)
readAndParse = parseIEAFloor <$> readData

lookupMapping :: IEA -> Mapping
lookupMapping iea = case (head iea, last iea) of
    (Dark, Light) -> Stable
    (Light, Dark) -> BiStable
    other -> Other


lookupIEAPixel :: IEA -> Int -> Pixel
lookupIEAPixel iea n = iea !! n

getNeighbourhood :: Position -> [Position]
getNeighbourhood (Position x y) = [Position (x-1) (y-1), Position x (y - 1), Position (x+1) (y - 1),
                                   Position (x - 1) y,   Position x y,       Position (x + 1) y,
                                   Position (x-1) (y+1), Position x (y + 1), Position (x+1) (y+1)]

lookupPL :: [(Position, a)] -> Position -> a -> a
lookupPL pas p d = snd $ fromMaybe (p, d) $ find (\l -> fst l == p) pas

reduceBinary :: [Int] -> Int
reduceBinary bs = foldl (\ b a -> b + fst a*2^snd a) 0 $ reverse bs `zip` [0..]

neighbourhoodToValue ::  Floor -> Pixel -> [Position] -> Int
neighbourhoodToValue floor repl = reduceBinary . map ((\pixel -> if pixel == Dark then 0 else 1) . (\p -> lookupPL floor p repl)) 


evolvePixel :: Position -> IEA -> Pixel -> Floor -> (Position, Pixel)
evolvePixel p iea repl floor = (p,) $ lookupIEAPixel iea $ neighbourhoodToValue floor repl $ getNeighbourhood p

onlyLight :: Floor -> Floor
onlyLight = filter (\l -> snd l == Light)

howManyLit :: Floor -> Int 
howManyLit = length . onlyLight

-- IEA is bi-stable for completely empty neighbourhoods
-- so instead of a set we need to completely fill a new boundary around the grid for each step
-- for the bistable examples we need to alternate between a boundary of 1 and a boundary of 0 to account for flashing!
wrapNewBoundary :: Floor -> Floor
wrapNewBoundary floor = floor ++ [(Position (minX-1) y, repl) | y <- [(minY-1) .. (maxY+1)]] ++ [(Position (maxX+1) y, repl) | y <- [(minY-1) .. (maxY+1)]] ++ [(Position x (maxY+1), repl) | x <- [minX .. maxX]] ++ [(Position x (minY-1), repl) | x <- [minX .. maxX]]
    where
        minP = minimumBy (compare `on` fst) floor
        maxP = maximumBy (compare `on` fst) floor
        minX = (\(Position x _) -> x) (fst minP)
        maxX = (\(Position x _) -> x) (fst maxP)
        minY = (\(Position _ y) -> y) (fst minP)
        maxY = (\(Position _ y) -> y) (fst maxP)
        repl = Dark

enhance :: IEA -> Int -> Floor -> Floor
enhance iea n floor = map ((\p -> evolvePixel p iea (repl m n) floor) . fst) $ wrapNewBoundary floor
    where
        m = lookupMapping iea
        repl Stable _ = Dark
        repl BiStable n = if even n then Light else Dark
        repl Other _ = error "oh no!"

runPt1 :: (IEA, Floor) -> Int
runPt1 (iea, floor) = howManyLit $ enhance iea 2 $ enhance iea 1 floor

{-
real	0m6.949s
user	0m6.934s
sys	    0m0.060s
-}



--This will not work in reasonable time on the input
--need to look at another data structure like zippers?
enhanceTimes :: IEA -> Int -> Floor -> Floor
enhanceTimes iea n = enhanceTimes' iea 1 (n+1)
    where
        enhanceTimes' iea m n f | m == n = f
                                | otherwise = enhanceTimes' iea (m+1) n (enhance iea m f)

runPt2 :: (IEA, Floor) -> Int
runPt2 (iea, floor) = howManyLit $ enhanceTimes iea 50 floor

solution :: IO Int
solution = runPt2 <$> readAndParse