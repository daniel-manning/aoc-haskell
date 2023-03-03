module Day04_2016 (
) where


import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, sepBy1, string, (<|>), letter)
import Data.Either.Combinators ( fromRight' )
import Data.Char (ord, chr)
import Data.List (group, sort, sortOn, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (Down(..), comparing)

data Room = Room {
    name :: [String],
    roomId :: Int,
    checksum:: String
} deriving (Show, Eq, Ord)

parseName :: String -> [String]
parseName = filter (not . null) . splitOn "-"


parseRoom :: Parser Room
parseRoom = do
    n <- many1 (letter <|> char '-')
    id <- many1 digit
    char '['
    c <- many1 letter
    char ']'
    return $ Room (parseName n) (read id) c

readData :: IO [String]
readData = lines <$> readFile "resource/2016/day04"

readAndParse = map (fromRight' . parse parseRoom "") <$> readData
------------------
isValid :: Room -> Bool
isValid (Room name _ checksum) = k == checksum
    where
        k = take 5 $ map snd $ sortOn (Down . fst) $ map (\k -> (length k, head k)) $ group $ sort $ concat name

runPt1 = sum . map roomId . filter isValid <$> readAndParse
--------------------

letterPlace c = ord c - 96
placeLetter n = chr (n + 96)

moveLetter k ' ' = ' '
moveLetter k c | z == 0 = placeLetter 26
               | otherwise = placeLetter z
    where 
        z = (k + letterPlace c) `mod` 26 

decryptMessage k = map (moveLetter k)
decryptRoom (Room ns id _) = map (decryptMessage id) ns

searchForRoom = filter (\r -> decryptRoom r == ["northpole","object","storage"])

runPt2 = head . map roomId . searchForRoom . filter isValid <$> readAndParse