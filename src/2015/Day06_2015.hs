module Day06_2015
    (
      day06Pt1,
      day06Pt2
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List (nub, foldl')
    import Data.Maybe (fromJust)
    import ListUtils
    import qualified Data.HashMap.Strict as H

    data Range = Range Int Int deriving Show
    data Rectangle = Rectangle Range Range deriving Show
    data Command = ON | OFF | TOGGLE deriving Show
    data Instruction = Instruction Command Rectangle deriving Show
    data LightState = LightOn | LightOff deriving (Ord, Eq, Show)
    newtype LightLevel = LightLevel Int deriving Show

    --I need better parser combinators knowledge to clean this up
    toCommand "turn on " = ON
    toCommand "turn off " = OFF
    toCommand "toggle " = TOGGLE

    --parse instructions to make commands
    rangeCoordinate :: Parser Int
    rangeCoordinate = do
      n <- many1 digit
      return $ read n

    parseRectangle :: Parser Rectangle
    parseRectangle = do
        xLower <- rangeCoordinate
        char ','
        yLower <- rangeCoordinate
        string " through "
        xHigher <- rangeCoordinate
        char ','
        Rectangle (Range xLower xHigher) . Range yLower <$> rangeCoordinate

    parseCommand :: Parser Command
    parseCommand = do
       n <- many1 (letter <|> space)
       return $ toCommand n

    parseInstruction = do
       command <- parseCommand
       Instruction command <$> parseRectangle

    --from a list of instructions make a list of positions from all ranges
    pointsFromRectangle :: Rectangle -> [Position]
    pointsFromRectangle (Rectangle (Range xl xh) (Range yl yh)) = [Position x y | x <- [xl..xh], y <-[yl..yh]]

    isInRectangle :: Position -> Rectangle -> Bool
    isInRectangle (Position x y) (Rectangle (Range xl xh) (Range yl yh)) = (x >= xl && x <= xh) && (y >= yl && y <= yh)

    --from list of positions work through each instruction to find which have affected and update state
    applyInstructions :: (Command -> Maybe a -> a) -> H.HashMap Position a -> [Instruction] -> H.HashMap Position a
    applyInstructions _ map [] = map
    applyInstructions apply map ((Instruction c r):is) = applyInstructions apply updated is
      where
        updated = foldl' (\m p -> H.insert p (apply c (H.lookup p map)) (H.delete p m)) map points
        points = pointsFromRectangle r

    runInstructions :: (Command -> Maybe a -> a) -> [Instruction] -> H.HashMap Position a
    runInstructions apply = applyInstructions apply H.empty

  
    applyCommand :: Command -> Maybe LightState -> LightState
    applyCommand ON     _        = LightOn
    applyCommand OFF    _        = LightOff
    applyCommand TOGGLE (Just LightOn)  = LightOff
    applyCommand TOGGLE (Just LightOff) = LightOn
    applyCommand TOGGLE Nothing  = LightOn

    toInstruction :: String -> Instruction
    toInstruction string  = fromRight' $ parse parseInstruction "" string

    day06Pt1 = length . (H.filter (== LightOn) . runInstructions applyCommand) . map toInstruction <$> readInstructions

    ----
    applyBrightnessCommand :: Command -> Maybe LightLevel -> LightLevel
    applyBrightnessCommand ON     (Just (LightLevel n)) = LightLevel (n + 1)
    applyBrightnessCommand ON     Nothing               = LightLevel 1
    applyBrightnessCommand OFF    (Just (LightLevel 0)) = LightLevel 0
    applyBrightnessCommand OFF    Nothing               = LightLevel 0
    applyBrightnessCommand OFF    (Just (LightLevel n)) = LightLevel (n - 1)
    applyBrightnessCommand TOGGLE (Just (LightLevel n)) = LightLevel (n + 2)
    applyBrightnessCommand TOGGLE Nothing               = LightLevel 2

    day06Pt2 =  sum . map (\(LightLevel n) -> n) . H.elems . (H.filter (\(LightLevel n) -> n > 0) . runInstructions applyBrightnessCommand) . map toInstruction <$> readInstructions

    readInstructions :: IO [String]
    readInstructions = lines <$> readFile "resource/2015/day06"

    {--
    PT1
    569999

    real	1m27.980s
    user	3m6.696s
    sys	1m20.077s
    --}

    {--
    PT2
    17836115

    real	1m36.266s
    user	3m20.851s
    sys	1m22.725s
    
    --}