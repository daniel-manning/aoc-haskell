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
                t = moveTail h t'
        updateTail' (r': r : rs) = Rope h t : updateTail' ((Rope h t) : rs)
            where
                h = (\(Rope a b) -> a) r
                t' = (\(Rope a b) -> b) r'
                t = moveTail h t'

moveTail ::Position -> Position -> Position
moveTail (Position hx hy) (Position tx ty) | hx == tx && hy - ty  >= 2 = Position tx (ty + 1)
                                                   | hx == tx && ty - hy  >= 2 = Position tx (ty - 1)
                                                   | hy == ty && hx - tx  >= 2 = Position (tx+1) ty
                                                   | hy == ty && tx - hx  >= 2 = Position (tx-1) ty
                                                   | hy - 1 == ty && tx - hx  >= 2 = Position (tx-1) (ty+1)
                                                   | hy - 1 == ty && hx - tx  >= 2 = Position (tx+1) (ty+1)
                                                   | ty - 1 == hy && hx - tx  >= 2 = Position (tx+1) (ty-1)
                                                   | ty - 1 == hy && tx - hx  >= 2 = Position (tx-1) (ty-1)
                                                   | hx - 1 == tx && ty - hy  >= 2 = Position (tx+1) (ty-1)
                                                   | hx - 1 == tx && hy - ty  >= 2 = Position (tx+1) (ty+1)
                                                   | tx - 1 == hx && hy - ty  >= 2 = Position (tx-1) (ty+1)
                                                   | tx - 1 == hx && ty - hy  >= 2 = Position (tx-1) (ty-1)
                                                   | otherwise = Position tx ty 
startingRope :: Rope
startingRope = Rope (Position 0 0) (Position 0 0)

countTailPositions :: [Rope] -> Int
countTailPositions = Set.size . Set.fromList . map (\(Rope h t) -> t)

runPt1 = countTailPositions . updateTail . ropeMoves startingRope <$> readAndParse
------------------
createNewRow :: [Rope] -> [Rope]
createNewRow = map (\(Rope p1 p2) -> Rope p2 (Position 0 0))

makeRopeSection = updateTail . createNewRow 

nineRopeSections :: [Rope] -> [Rope]
nineRopeSections = last . take 9 . iterate makeRopeSection

countMovesInCommand  (R n) = n
countMovesInCommand  (L n) = n
countMovesInCommand  (D n) = n
countMovesInCommand  (U n) = n

countMoves = sum . map countMovesInCommand

runPt2 = countTailPositions . nineRopeSections . updateTail . ropeMoves startingRope <$> readAndParse