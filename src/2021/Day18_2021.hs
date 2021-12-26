module Day18_2021 where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, (<|>))
import Data.Either.Combinators ( fromRight' )

data SnailFishNumber = Pair SnailFishNumber SnailFishNumber | Value Int

instance Show SnailFishNumber where
    show (Value n) = show n
    show (Pair a b) = "["++show a ++ ", " ++ show b ++ "]"

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
readData = lines <$> readFile "resource/2021/day18_test"

readAndParse :: IO [SnailFishNumber]
readAndParse = map (fromRight' . parse parsePair "") <$> readData
--------------------

{- TODO -}
explode :: SnailFishNumber -> SnailFishNumber
explode = id

{- TODO -}
splitSnailFishNumber :: SnailFishNumber -> SnailFishNumber
splitSnailFishNumber = id

hasValueGTE10 :: SnailFishNumber -> Bool 
hasValueGTE10 (Value n) | n >= 10 = True 
                        | otherwise = False
hasValueGTE10 (Pair a b) =  hasValueGTE10 a || hasValueGTE10 b

hasNestedFourDeep :: SnailFishNumber -> Bool
hasNestedFourDeep = hasNested 4

hasNested :: Int -> SnailFishNumber -> Bool
hasNested 0 (Value _) = True
hasNested n (Value _) = False
hasNested 0 (Pair a b) = True
hasNested n (Pair a b) = hasNested (n-1) a || hasNested (n-1) b

reduce :: SnailFishNumber -> SnailFishNumber
reduce sfn | hasNestedFourDeep sfn = reduce $ explode sfn
           | hasValueGTE10 sfn = reduce $ splitSnailFishNumber sfn
           | otherwise = sfn

add :: SnailFishNumber -> SnailFishNumber -> SnailFishNumber
add x y = reduce $ Pair x y