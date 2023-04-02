module Day08_2016 (
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators

import Data.Functor (($>))
import Data.List (foldl', maximum)
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HashMap

data Polarity = Inc | Dec deriving Show
data Comparator = GT_ | LT_ | GTE_ | LTE_ | E_ | NE_ deriving Show
data Test = Test String Comparator Int deriving Show
data RegisterOp = RegisterOp {
    registerName :: String,
    polaity:: Polarity,
    change :: Int,
    test :: Test 
} deriving Show

type Register = HashMap.HashMap String Int

parseGT :: Parser Comparator
parseGT = string " > " $> GT_

parseLT :: Parser Comparator
parseLT = string " < " $> LT_

parseGTE :: Parser Comparator
parseGTE = string " >= " $> GTE_

parseLTE :: Parser Comparator
parseLTE = string " <= " $> LTE_

parseE :: Parser Comparator
parseE = string " == " $> E_

parseNE :: Parser Comparator
parseNE = string " != " $> NE_

parseComparator :: Parser Comparator
parseComparator = try parseGT <|> try parseLT <|> try parseGTE <|> try parseLTE <|> try parseE <|> parseNE

parseTest_ :: Parser Test
parseTest_ = do
    string "if "
    rn <- many1 letter
    c <- parseComparator
    v <- many1 (char '-' <|> digit)
    return $ Test rn c (read v)

parseInc :: Parser Polarity
parseInc = string " inc " $> Inc

parseDec :: Parser Polarity
parseDec = string " dec " $> Dec

parsePolarity :: Parser Polarity
parsePolarity = try parseInc <|> try parseDec

parseRegisterOp :: Parser RegisterOp
parseRegisterOp = do
    rn <- many1 letter
    p <- parsePolarity
    v <- many1 (char '-' <|> digit)
    string " "
    t <- parseTest_
    return $ RegisterOp rn p (read v) t

readData :: IO [String]
readData = lines <$> readFile "resource/2017/day08"

readAndParse = map (fromRight' . parse parseRegisterOp "") <$> readData
-------------------------
initialRegisters :: [RegisterOp] -> Register
initialRegisters = foldl' (\hs s -> HashMap.insert s 0 hs) HashMap.empty . map registerName

runTest :: Test -> Int -> Bool
runTest (Test _ GT_ b) a = a > b
runTest (Test _ LT_ b) a = a < b
runTest (Test _ GTE_ b) a = a >= b
runTest (Test _ LTE_ b) a = a <= b
runTest (Test _ E_ b) a = a == b
runTest (Test _ NE_ b) a = a /= b

alterValue :: Polarity -> Int -> Int -> Maybe Int
alterValue Inc v n = Just (n + v)
alterValue Dec v n = Just (n - v)

runRegisterOp :: Register ->  RegisterOp -> Register
runRegisterOp hm (RegisterOp rn p v t) | runTest t k = HashMap.update (alterValue p v) rn hm
                   | otherwise = hm
    where
        registerValue (Test rn _ _) = HashMap.lookup rn hm
        k = fromJust $ registerValue t

runOps :: [RegisterOp] -> Register
runOps ros = foldl' runRegisterOp (initialRegisters ros) ros

largestRegisterValue :: Register -> Int
largestRegisterValue = maximum . HashMap.elems

day08pt1 = largestRegisterValue . runOps <$> readAndParse
----------------

runOpAndUpdateMaximum :: (Register, Int) -> RegisterOp -> (Register, Int)
runOpAndUpdateMaximum (r, m1) ro = (r', max m1 m2)
    where
        r' = runRegisterOp r ro
        m2 = largestRegisterValue r' 

runOps' :: [RegisterOp] -> (Register, Int)
runOps' ros = foldl' runOpAndUpdateMaximum (initialRegisters ros, 0) ros

day08pt2 = snd . runOps' <$> readAndParse
