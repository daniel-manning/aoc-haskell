module Day03_2024 (
) where

import Text.Regex.TDFA
import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import Data.List (sortBy)
import Data.Ord (comparing)

data Mul = Mul Int Int deriving Show
data Instruction = DO | DONT | INST Mul deriving Show
-----------
parseMul :: Parser Mul
parseMul = do
    string "mul("
    x <- many1 digit
    string ","
    y <- many1 digit
    return $ Mul (read x) (read y)

readData :: IO String
readData = readFile "resource/2024/day03"

mulRegex = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

matches s = getAllTextMatches $ (s =~ mulRegex) :: [String]

readMul :: String -> [Mul]
readMul = map (fromRight' . parse parseMul "") . matches

multiply (Mul m n) = m*n

runPt1 = sum . map multiply . readMul <$> readData
------------
doRegex = "do\\(\\)"
dontRegex = "don't\\(\\)"

mulIndexes :: String -> [(Int, Instruction)]
mulIndexes s = zip (map fst ind) muls
    where
       ind = getAllMatches $ (s =~ mulRegex) :: [(Int, Int)]
       muls = map INST $ readMul s

doIndexes s = map (\l -> (fst l, DO)) $ (getAllMatches (s =~ doRegex) :: [(Int, Int)])
dontIndexes s = map (\l -> (fst l, DONT)) $ (getAllMatches (s =~ dontRegex) :: [(Int, Int)])

allCases s = map snd $ sortBy (comparing fst) $ mulIndexes s ++ doIndexes s ++ dontIndexes s

stateTracking :: (Instruction, [Mul]) -> Instruction -> (Instruction, [Mul])
stateTracking (_, is) DO = (DO, is)
stateTracking (_, is) DONT = (DONT, is)
stateTracking (DO, is) (INST m) = (DO, is ++ [m])
stateTracking (DONT, is) (INST m) = (DONT, is)

assembleInstructions = foldl stateTracking (DO, [])

runPt2 = sum . map multiply . snd . assembleInstructions . allCases <$> readData
