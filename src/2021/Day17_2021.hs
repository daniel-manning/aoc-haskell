{-#  LANGUAGE TupleSections #-}
module Day17_2021 where

import Text.ParserCombinators.Parsec (many1, letter, Parser, parse, char, string, (<|>), digit)
import Data.Either.Combinators ( fromRight' )
import Data.String.Utils (rstrip)
import Data.Function (on)
import Data.List (sort, nub, maximumBy)

data Target = Target (Int, Int) (Int, Int) deriving Show

parseTarget:: Parser Target
parseTarget = do
    string "target area: x="
    x1 <- many1 (char '-' <|> digit)
    string ".."
    x2 <- many1 (char '-' <|> digit)
    string ", y="
    y1 <- many1 (char '-' <|> digit)
    string ".."
    y2 <- many1 (char '-' <|> digit)
    return $ Target (read x1, read x2) (read y1, read y2)

readData :: IO String
readData = rstrip <$> readFile "resource/2021/day17"
-------------------

readAndParse :: IO Target
readAndParse = fromRight' . parse parseTarget "" <$> readData


rootsOfTimeY :: Int -> (Int, Int) -> [Double]
rootsOfTimeY v (ymin, ymax) = sort $ filter (>= 0) $ r v =<< [ymin, ymax]

r :: Int -> Int -> [Double]
r v' y' = [(2*v + 1 + sqrt(4*v*v + 4*v + 1 - 8*y))/2, (2*v + 1 - sqrt(4*v*v + 4*v + 1 - 8*y))/2]
    where
        v = fromIntegral v'
        y = fromIntegral y'

integersInRange :: [Double] -> [Int]
integersInRange ds = [a .. b]
    where
        a = ceiling $ head ds
        b = floor $ last ds

reachedFullRange :: Int -> (Int, Int) -> [Int]
reachedFullRange t (xMin, xMax) = map floor $ filter (\v -> xMin' <= (0.5*v*(v+1)) && xMax' >= (0.5*v*(v+1))) [0 .. t']
    where
        xMin' = fromIntegral xMin
        xMax' = fromIntegral xMax
        t' = fromIntegral t

stillDecelerating :: Int -> (Int, Int) -> [Int]
stillDecelerating t (xMin, xMax) = filter (>=t) $ integersInRange [(2*xMin' + t'*(t'-1))/(2*t'), (2*xMax' + t'*(t'-1))/(2*t')]
    where
        xMin' = fromIntegral xMin
        xMax' = fromIntegral xMax
        t' = fromIntegral t

extrapolateXVelocity :: Int -> (Int, Int) -> [Int]
extrapolateXVelocity t xRange = nub $ sort $ reachedFullRange t xRange ++ stillDecelerating t xRange

constructVelocity :: Target -> Int -> [(Int, Int)]
constructVelocity (Target xRange yRange) vY = map (, vY) $ nub $ sort $ (`extrapolateXVelocity` xRange) =<< integersInRange (rootsOfTimeY vY yRange)

{- 
    This is just the heighest y velocity assuming it doesnt hit the target before dropping (ie traget needs to be in -y range)
    When it hits zero again it will have velocity -Vy then the next step is -Vy-1 so Vy needs to be one smaller than the lowest edge of the target
    to avoid overshooting in a single step
 -}
maximumHeight :: Target -> Int
maximumHeight t@(Target xRange (yMin, yMax))= (\v -> v*(v+1) `div` 2) . snd . head $ constructVelocity t (abs yMin - 1)

runPt1 :: Target -> Int
runPt1 = maximumHeight
----------------

{- assuming a negative yMin -}
allVelocities :: Target -> [(Int, Int)]
allVelocities t@(Target xRange (yMin, yMax)) = nub $ concatMap (constructVelocity t) [(yMin -1) .. (abs yMin)]

runPt2 :: Target -> Int 
runPt2 = length . allVelocities

run :: IO Int
run = runPt2 <$> readAndParse
