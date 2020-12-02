module Day02
    (
        totalValidPasswords
    ) where

import Text.ParserCombinators.Parsec
import Data.List (group, sort)
import Data.Maybe
import Data.Either.Combinators

data Rule = Rule Int Int Char deriving Show
data Password = Password String deriving Show

ruleNumber :: Parser Int
ruleNumber = do
  n <- many1 digit
  return $ read n

password ::  Parser String
password = many anyChar

parseRuleAndPassword = do
  lower <- ruleNumber
  char '-'
  upper <- ruleNumber
  space
  character <- anyChar
  char ':'
  space
  textPassword <- password
  return $ (Rule lower upper character, Password textPassword)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

checkPasswordForOldRule :: (Rule, Password) -> Bool
checkPasswordForOldRule ((Rule lowest highest character), (Password password)) =
  (isJust noOfUses) && (highest >= (snd $ fromJust noOfUses)) && (lowest <= (snd $ fromJust noOfUses))
  where
    noOfUses = listToMaybe $ filter (\n -> character == fst n) $ map (\x -> (head x, length x)) $ group $ sort password

checkPasswordForRule :: (Rule, Password) -> Bool
checkPasswordForRule ((Rule posOne posTwo character), (Password password)) =
   (passwordCharacter posOne character) /= (passwordCharacter posTwo character)
  where
   passwordCharacter n c = (fst $ head $ filter (\l -> (snd l) == n ) $ passwordWithIndex) == c
   passwordWithIndex = zip password ([1..])

validatePasswordWithRule :: String -> Bool
validatePasswordWithRule input = checkPasswordForRule $ fromRight' $ regularParse parseRuleAndPassword input

totalValidPasswords :: IO Int
totalValidPasswords =  length <$> filter id <$> map validatePasswordWithRule <$> readFileOfPasswords

readFileOfPasswords :: IO [String]
readFileOfPasswords = lines <$> readFile "resource/day02"
