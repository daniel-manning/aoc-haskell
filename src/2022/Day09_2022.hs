module Day09_2022 (
) where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, string, sepBy1)
import Data.Either.Combinators (fromRight')
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import qualified Data.Set as Set

data Move = R Int | L Int | D Int | U Int deriving Show
data Position = Position {
    x :: Int,
    y :: Int
} deriving (Show, Eq, Ord)

data Rope = Rope {
    h :: Position,
    t :: Position
} deriving Show

parseRight :: Parser Move
parseRight = do
    string "R "
    n <- many1 digit
    return $ R (read n)

parseLeft :: Parser Move
parseLeft = do
    string "L "
    n <- many1 digit
    return $ L (read n)

parseUp :: Parser Move
parseUp = do
    string "U "
    n <- many1 digit
    return $ U (read n)

parseDown :: Parser Move
parseDown = do
    string "D "
    n <- many1 digit
    return $ D (read n)

readData :: IO [String]
readData = lines <$> readFile "resource/2022/day09"

parseMove= parseRight <|> parseLeft <|> parseUp <|> parseDown

readAndParse = map (fromRight' . parse parseMove "") <$> readData
--------------
moveRope :: Rope -> Move -> [Rope]
moveRope (Rope (Position x y) p) (U n) = map (\i -> Rope (Position x (y + i)) p) [0..n]
moveRope (Rope (Position x y) p) (D n) = map (\i -> Rope (Position x (y - n)) p) [0..n]
moveRope (Rope (Position x y) p) (L n) = map (\i -> Rope (Position (x-i) y) p) [0..n]
moveRope (Rope (Position x y) p) (R n) = map (\i -> Rope (Position (x+i) y) p) [0..n]


ropeMoves :: Rope -> [Move] -> [Rope]
ropeMoves = scanlistEnds moveRope

scanlistEnds :: (a -> b -> [a]) -> a -> [b] -> [a]
scanlistEnds f q = scanlGo f [q]
  where
    scanlGo           :: (a -> b -> [a]) -> [a] -> [b] -> [a]
    scanlGo f q ls    = tail q  ++  (case ls of
                               []   -> []
                               x:xs -> scanlGo f (f (last q) x) xs)

--easier if the tail moves as a consequence
updateTail :: [Rope] -> [Rope]
updateTail rs = head rs : updateTail' rs
    where
        updateTail' [r', r] = [Rope h t]
            where
                h = (\(Rope a b) -> a) r
                t' = (\(Rope a b) -> b) r'
                t = fromJust $ moveTail h t'
        updateTail' (r': r : rs) = Rope h t : updateTail' ((Rope h t) : rs)
            where
                h = (\(Rope a b) -> a) r
                t' = (\(Rope a b) -> b) r'
                t = fromJust $ moveTail h t'

moveTail ::Position -> Position -> Maybe Position
moveTail (Position hx hy) (Position tx ty) | hx == tx && hy - ty  >= 2 = Just $ Position tx (ty + 1)
                                                   | hx == tx && ty - hy  >= 2 = Just $ Position tx (ty - 1)
                                                   | hy == ty && hx - tx  >= 2 = Just $ Position (tx+1) ty
                                                   | hy == ty && tx - hx  >= 2 = Just $ Position (tx-1) ty
                                                   | hy - ty >= 1 && tx - hx  >= 2 = Just $ Position (tx-1) (ty+1)
                                                   | hy - ty >= 1 && hx - tx  >= 2 = Just $ Position (tx+1) (ty+1)
                                                   | ty - hy >= 1 && hx - tx  >= 2 = Just $ Position (tx+1) (ty-1)
                                                   | ty - hy >= 1 && tx - hx  >= 2 = Just $ Position (tx-1) (ty-1)
                                                   | hx - tx >= 1 && ty - hy  >= 2 = Just $ Position (tx+1) (ty-1)
                                                   | hx - tx >= 1 && hy - ty  >= 2 = Just $ Position (tx+1) (ty+1)
                                                   | tx - hx >= 1 && hy - ty  >= 2 = Just $ Position (tx-1) (ty+1)
                                                   | tx - hx >= 1 && ty - hy  >= 2 = Just $ Position (tx-1) (ty-1)
                                                   | abs (hx - tx) <= 1 &&  abs (hy - ty) <= 1= Just $ Position tx ty 
                                                   | otherwise = Nothing --Position tx ty 
startingRope :: Rope
startingRope = Rope (Position 0 0) (Position 0 0)

countTailPositions :: [Rope] -> Int
countTailPositions = Set.size . Set.fromList . map (\(Rope h t) -> t)

runPt1 = countTailPositions . updateTail . ropeMoves startingRope <$> readAndParse
------------------

replicateRopeLayer :: [Rope] -> [Rope]
replicateRopeLayer = map (\(Rope _ b) -> Rope b (Position 0 0))

nextLayer = updateTail . replicateRopeLayer

endOfFullRope :: [Rope] -> [Rope]
endOfFullRope rs = iterate nextLayer rs !! 8

runPt2 = countTailPositions . endOfFullRope . updateTail . ropeMoves startingRope <$> readAndParse