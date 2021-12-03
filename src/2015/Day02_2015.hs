module Day02_2015
    (
    sumOfElvesList
    ) where

import Text.ParserCombinators.Parsec
import Data.List (sort)
import Data.Either.Combinators

data Dimensions = Dimensions Int Int Int

number :: Parser Int
number = do
  n <- many1 digit
  return $ read n

parseDimensions = do
  l <- number
  char 'x'
  w <- number
  char 'x'
  h <- number
  return (Dimensions l w h)


surfaceArea :: Dimensions -> Int
surfaceArea (Dimensions l w h) = 2*l*w + 2*w*h + 2*h*l + slack (l,w,h)

slack :: (Int, Int, Int) -> Int
slack (l,w,h) = product $ take 2 $ sort [l,w,h]

calculateArea :: String -> Int
calculateArea input = surfaceArea $ fromRight' $ parse parseDimensions "" input

calculateLength :: String -> Int
calculateLength input = ribbon $ fromRight' $ parse parseDimensions "" input

ribbon :: Dimensions -> Int
ribbon (Dimensions l w h) = l*w*h + 2 * sum (take 2 $ sort [l,w,h])

sumOfElvesList = sum . map calculateArea <$> readString
sumOfRibbonList = sum . map calculateLength <$> readString

readString :: IO [String]
readString = lines <$> readFile "resource/2015/day02"