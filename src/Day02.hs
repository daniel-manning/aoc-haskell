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

checkPasswordForRule :: (Rule, Password) -> Bool
checkPasswordForRule ((Rule lowest highest character), (Password password)) =
  (isJust noOfUses) && (highest >= (snd $ fromJust noOfUses)) && (lowest <= (snd $ fromJust noOfUses))
  where
    noOfUses = listToMaybe $ filter (\n -> character == fst n) $ map (\x -> (head x, length x)) $ group $ sort password

validatePasswordWithRule :: String -> Bool
validatePasswordWithRule input = checkPasswordForRule $ fromRight' $ regularParse parseRuleAndPassword input

totalValidPasswords :: IO Int
totalValidPasswords =  length <$> filter id <$> map validatePasswordWithRule <$> readFileOfPasswords

readFileOfPasswords :: IO [String]
readFileOfPasswords = lines <$> readFile "resource/day02"
