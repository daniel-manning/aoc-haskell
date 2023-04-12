module Day10_2016 (
) where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, string, sepBy1, space, alphaNum, try, letter)
import Data.Either.Combinators (fromRight')
import Control.Applicative ((<|>))
import Data.List (find, nub, sort)
import Data.Maybe (fromJust)
import Data.String.Utils (startswith)

data Instruction = Setup String Int | BotInstruction String String String deriving Show

parseSetup :: Parser Instruction
parseSetup = do
    string "value "
    v <- many1 digit
    string " goes to "
    bot <- many1 (space <|> alphaNum)
    return $ Setup bot (read v)

parseBotInstruction :: Parser Instruction
parseBotInstruction = do
    string "bot "
    v <- many1 digit
    string " gives low to "
    botLow <- many1 letter
    string " "
    vLow <- many1 digit
    string " and high to "
    botHigh <- many1 letter
    string " "
    vHigh <- many1 digit
    return $ BotInstruction ("bot "++v) (botLow ++ " " ++ vLow) (botHigh ++ " " ++ vHigh)

parseInstruction = try parseSetup <|> try parseBotInstruction

readData :: IO [String]
readData = lines <$> readFile "resource/2016/day10"

readAndParse :: IO [Instruction]
readAndParse = map (fromRight' . parse parseInstruction "") <$> readData
-------------------------

isSetup :: Instruction -> Bool
isSetup (Setup _ _) = True
isSetup _           = False

data Bot = Bot {
    nameB :: String,
    low :: Maybe Int,
    high :: Maybe Int
 } deriving Show

data Output = Output {
   nameO :: String,
   values :: [Int] 
} deriving Show

update :: Bot -> Int -> Bot
update (Bot s Nothing Nothing) n              = Bot s (Just n) Nothing
update (Bot s (Just k) Nothing) n | k < n     = Bot s (Just k) (Just n)
                                  | otherwise = Bot s (Just n) (Just k)

updateOutput :: Output -> Int -> Output
updateOutput (Output s xs) x = Output s (x:xs)

updateOutput' :: [Output] -> String -> Int -> [Output]
updateOutput' os s n = map (\o -> if s == nameO o then updateOutput o n  else o) os

updateBot :: [Bot] -> Instruction -> [Bot]
updateBot bs (Setup n k) = map (\b -> if n == nameB b then update b k  else b) bs

updateBot' :: [Bot] -> String -> Int -> [Bot]
updateBot' bs s n = map (\b -> if s == nameB b then update b n  else b) bs

extractNames (Setup s _) = [s]
extractNames (BotInstruction a b c) = [a,b,c]

botnames :: [Instruction] -> [String]
botnames bs = sort $ nub $ extractNames =<< bs 

setUpAgents :: [String] -> ([Bot], [Output])
setUpAgents ns = (bs, os)
    where
        bs = map (\n -> Bot n Nothing Nothing) $ filter (startswith "bot") ns
        os = map (`Output` []) $ filter (startswith "output") ns

runSetupSteps :: [Instruction] -> ([Bot], [Output]) -> ([Bot], [Output])
runSetupSteps is (bs, os) = (bs', os)
    where
        ss = filter isSetup is
        bs' = foldl updateBot bs ss

hasTwoValues (Bot _ (Just _) (Just _))  = True
hasTwoValues _                          = False

placeValue :: ([Bot], [Output]) -> String -> Int -> ([Bot], [Output])
placeValue (bs, os) s n | startswith "bot" s = (updateBot' bs s n, os)
                        | otherwise          = (bs, updateOutput' os s n)

runInstruction :: ([Bot], [Output]) -> Bot -> Instruction -> ([Bot], [Output])
runInstruction (bs, os) (Bot _ (Just a) (Just b)) (BotInstruction n l h) = (bs''', os'')
    where
        (bs', os') = placeValue (bs, os) l a
        (bs'', os'') = placeValue (bs', os') h b
        bs''' = Bot n Nothing Nothing : filter (\bot -> n /= nameB bot) bs''

run :: [Instruction] -> ([Bot], ([Bot], [Output])) -> ([Bot], ([Bot], [Output]))
run is (hs, (bs, os)) | any hasTwoValues bs = run is (hs', (bs', os'))
                      | otherwise = (hs, (bs, os))
    where
        bis = filter (not.isSetup) is
        active = head $ filter hasTwoValues bs
        iActive = head $ filter (\(BotInstruction n _ _) -> n == nameB active) bis
        hs' = active : hs
        (bs', os') = runInstruction (bs, os) active iActive

findMatchingBot :: (Int, Int) -> [Bot] -> Maybe Bot
findMatchingBot t = find (\(Bot n (Just l) (Just h)) -> (l,h) == t)

setupAndRun :: [Instruction] -> ([Bot], ([Bot], [Output]))
setupAndRun is = (\l -> run is ([], l)) $ runSetupSteps is $ setUpAgents $ botnames is

--trial = (2,5)
trial = (17, 61)

runPt1 = nameB . fromJust . findMatchingBot trial . fst .  setupAndRun <$> readAndParse

------------

extractOutput :: [Output] -> String -> Int
extractOutput os n = head $ values $ fromJust $ find (\o -> n == nameO o) os

runPt2 = product . (\os -> map (extractOutput os) ["output 0", "output 1", "output 2"]) . snd . snd . setupAndRun <$> readAndParse