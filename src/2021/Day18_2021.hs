module Day18_2021 where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char)
import Data.Either.Combinators ( fromRight' )
import Control.Applicative
import Data.Maybe (fromJust, fromMaybe)
import Data.List (foldl1')

--liberally ripped off from https://work.njae.me.uk/2021/12/21/advent-of-code-2021-day-18/

data SnailFishNumber = Pair SnailFishNumber SnailFishNumber | Value Int
    deriving Eq

instance Show SnailFishNumber where
    show (Value n) = show n
    show (Pair a b) = "["++show a ++ ", " ++ show b ++ "]"

data Ctx = Top | L Ctx SnailFishNumber | R SnailFishNumber Ctx

type Loc = (SnailFishNumber, Ctx)

parseValue :: Parser SnailFishNumber
parseValue = do
    n <- many1 digit 
    return $ Value (read n)

parsePair :: Parser SnailFishNumber
parsePair = do
    char '['
    snailFishL <- parseValue <|> parsePair
    char ','
    snailFishR <- parseValue <|> parsePair
    char ']'
    return $ Pair snailFishL snailFishR

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day18"

readAndParse :: IO [SnailFishNumber]
readAndParse = map (fromRight' . parse parsePair "") <$> readData
--------------------
left :: Loc -> Loc
left (Pair l r, c) = (l, L c r)

right :: Loc  -> Loc
right (Pair l r, c) = (r, R l c)

top :: SnailFishNumber -> Loc
top t = (t, Top)

up :: Loc -> Loc
up (t, L c r) = (Pair t r, c)
up (t, R l c) = (Pair l t, c)

upmost :: Loc -> Loc
upmost l@(t, Top) = l
upmost l = upmost (up l)

modify :: Loc -> (SnailFishNumber -> SnailFishNumber) -> Loc
modify (t, c) f = (f t, c)
--------------------
splitSnailFishNumber :: SnailFishNumber -> Maybe Loc
splitSnailFishNumber sfn = split' (top sfn)

split' :: Loc -> Maybe Loc
split' t@(Value n, _) | n >= 10 = Just t
                     | otherwise = Nothing
split' p@(Pair _ _, _) = split' (left p) <|> split' (right p) 

splitValue :: Int -> SnailFishNumber
splitValue n = Pair (Value l)  (Value r)
    where
        l = n `div` 2
        r = n `div` 2 + n `mod` 2

split :: SnailFishNumber -> Maybe SnailFishNumber
split snf = fst . upmost . (\n -> modify n (value n)) <$> splitN
    where
        splitN = splitSnailFishNumber snf
        value s = (\_ -> (splitValue . (\(Value n, _) -> n)) s)

--------------
pairDepth :: Int -> SnailFishNumber -> Maybe Loc
pairDepth n snf = pairDepth' n (top snf)

pairDepth' :: Int -> Loc -> Maybe Loc
pairDepth' _ (Value _, _) = Nothing
pairDepth' 0 p@(Pair _ _, _) = Just p
pairDepth' n p@(Pair _ _, _) = pairDepth' (n-1) (left p) <|> pairDepth' (n-1) (right p)

rightmostOnLeft :: Loc -> Maybe Loc
rightmostOnLeft (_, Top) = Nothing
rightmostOnLeft t@(_, L c r) = rightmostOnLeft $ up t
rightmostOnLeft t@(_, R l c) = Just $ rightmostNum $ left $ up t

rightmostNum :: Loc -> Loc
rightmostNum t@(Value _, _) = t
rightmostNum t@(Pair _ _, _) = rightmostNum $ right t

leftmostOnRight :: Loc -> Maybe Loc
leftmostOnRight (_, Top) = Nothing
leftmostOnRight t@(_, R c r) = leftmostOnRight $ up t
leftmostOnRight t@(_, L l c) = Just $ leftmostNum $ right $ up t

leftmostNum :: Loc -> Loc
leftmostNum t@(Value _, _) = t
leftmostNum t@(Pair _ _, _) = leftmostNum $ left t

{- fix me: This could be better expressed -}
explode :: SnailFishNumber -> Maybe SnailFishNumber
explode num = (\_ -> num1) <$> mp0
    where 
    mp0 = pairDepth 4 num
    p0 = fromJust mp0
    ((Pair (Value nl) (Value nr)), _) = p0
    p1 = fromMaybe p0 $ ((\leftReg -> modify leftReg (\(Value n) -> Value (n + nl))) <$> rightmostOnLeft p0)
    p2 = fromMaybe p1 $ ((\rightReg -> modify rightReg (\(Value n) -> Value (n + nr))) <$> (pairDepth' 4 (upmost p1) >>= leftmostOnRight))
    p3 = fromMaybe p2 $ ((\centrePair -> modify centrePair (\_ -> Value 0)) <$> (pairDepth' 4 (upmost p2)))
    num1 = fst $ upmost p3

reduce :: SnailFishNumber -> SnailFishNumber
reduce sfn = fromMaybe sfn $ reduce <$> (explode sfn <|> split sfn)

add :: SnailFishNumber -> SnailFishNumber -> SnailFishNumber
add x y = reduce $ Pair x y

sumSnailList :: [SnailFishNumber] -> SnailFishNumber
sumSnailList = foldl1' add

magnatude :: SnailFishNumber -> Int
magnatude (Value n) = n
magnatude (Pair l r) = 3 * (magnatude l) + 2*(magnatude r)

runPt1 = magnatude . sumSnailList <$> readAndParse

runPt2 = (\ns -> maximum [magnatude $ add a b | a <- ns, b <- ns, a /= b]) <$> readAndParse