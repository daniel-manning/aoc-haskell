module Day02_2020
    (
        totalValidPasswords
    ) where

import Text.ParserCombinators.Parsec
import Data.List (group, sort, find)
import Data.Maybe
import Data.Either.Combinators

data Rule = Rule Int Int Char deriving Show
newtype Password = Password String deriving Show

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
  return (Rule lower upper character, Password textPassword)

checkPasswordForOldRule :: (Rule, Password) -> Bool
checkPasswordForOldRule (Rule lowest highest character, Password password) =
  isJust noOfUses && (highest >= snd (fromJust noOfUses)) && (lowest <= snd (fromJust noOfUses))
  where
    noOfUses = find (\ n -> character == fst n) $ map (\x -> (head x, length x)) $ group $ sort password

checkPasswordForRule :: (Rule, Password) -> Bool
checkPasswordForRule (Rule a b c, Password p) =
   passwordCharacter a c /= passwordCharacter b c
  where
   passwordCharacter n c = fst (head $ filter (\l -> snd l == n ) passwordWithIndex) == c
   passwordWithIndex = zip p [1..]

validatePasswordWithRule :: String -> Bool
validatePasswordWithRule input = checkPasswordForRule $ fromRight' $ parse parseRuleAndPassword "" input

validatePasswordWithOldRule :: String -> Bool
validatePasswordWithOldRule input = checkPasswordForOldRule $ fromRight' $ parse parseRuleAndPassword "" input

totalValidPasswords :: IO Int
totalValidPasswords =  length . filter id . map validatePasswordWithRule <$> readFileOfPasswords

totalValidOldPasswords :: IO Int
totalValidOldPasswords =  length . filter id . map validatePasswordWithOldRule <$> readFileOfPasswords

readFileOfPasswords :: IO [String]
readFileOfPasswords = lines <$> readFile "resource/2020/day02"
