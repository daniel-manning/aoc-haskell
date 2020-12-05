module Day06_2015
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List (nub)

    data Position = Position Int Int deriving (Eq, Show)
    data Range = Range Int Int deriving Show
    data Rectangle = Rectangle Range Range deriving Show
    data Command = ON | OFF | TOGGLE deriving Show
    data Instruction = Instruction Command Rectangle deriving Show
    data LightState = LightOn | LightOff deriving (Eq, Show)

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
        yHigher <- rangeCoordinate
        return (Rectangle (Range xLower xHigher) (Range yLower yHigher))

    parseCommand :: Parser Command
    parseCommand = do
       n <- many1 (letter <|> space)
       return $ toCommand n

    --parseCommand :: Parser Command
    --parseCommand = parseToggle <|> parseTurnCommand

    parseInstruction = do
       command <- parseCommand
       rectangle <- parseRectangle
       return (Instruction command rectangle)

    test input = runInstructions $ [(fromRight' $ parse parseInstruction "" input)]
    --from a list of instructions make a list of positions from all ranges
    pointsFromRectangle :: Rectangle -> [Position]
    pointsFromRectangle (Rectangle (Range xl xh) (Range yl yh)) = [Position x y | x <- [xl..xh], y <-[yl..yh]]

    gatherAffectedPoints [] = []
    gatherAffectedPoints ((Instruction c r): xs) = pointsFromRectangle r ++ gatherAffectedPoints xs

    isInRectangle :: Position -> Rectangle -> Bool
    isInRectangle (Position x y) (Rectangle (Range xl xh) (Range yl yh)) = (x >= xl && x <= xh) && (y >= yl && y <= yh)
    --from list of positions work through each instruction to find which have affected and update state
    lastStateOfPoint :: Position -> LightState -> [Instruction] -> LightState
    lastStateOfPoint _ state [] = state
    lastStateOfPoint p state ((Instruction c r): is) | isInRectangle p r = lastStateOfPoint p (applyCommand c state) is
                                                     | otherwise = lastStateOfPoint p state is

    runInstructions :: [Instruction] -> [(Position, LightState)]
    runInstructions instructions = map (\p -> (p, lastStateOfPoint p LightOff instructions)) points
       where points = nub $ gatherAffectedPoints instructions

    applyCommand :: Command -> LightState -> LightState
    applyCommand ON     _        = LightOn
    applyCommand OFF    _        = LightOff
    applyCommand TOGGLE LightOn  = LightOff
    applyCommand TOGGLE LightOff = LightOn

    toInstruction :: String -> Instruction
    toInstruction string  = fromRight' $ parse parseInstruction "" string

    day06Pt1 = (length . filter (\l -> snd l == LightOn) . runInstructions) <$> map toInstruction <$> readInstructions

    readInstructions :: IO [String]
    readInstructions = lines <$> readFile "resource/2015/day06"