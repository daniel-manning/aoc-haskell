module Day08_2020
    (
    day08Pt1
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators

    data Instruction = Acc Int | Jmp Int | Nop Int deriving Show
    data Accumulator = Accumulator Int deriving Show
    data ProgrammeState = ProgrammeState Int Accumulator deriving Show

    readWithSign:: Char -> String -> Int
    readWithSign '-' num = read ('-':num)
    readWithSign '+' num = read num

    parseAcc =  do
        string "acc "
        sign <- choice [char '+', char '-']
        num <- many1 digit
        return $ Acc (readWithSign sign num)

    parseJmp =  do
        string "jmp "
        sign <- choice [char '+', char '-']
        num <- many1 digit
        return $ Jmp (readWithSign sign num)

    parseNop =  do
        string "nop "
        sign <- choice [char '+', char '-']
        num <- many1 digit
        return $ Nop (readWithSign sign num)

    parseInstruction = (try parseAcc) <|> (try parseJmp) <|> (try parseNop)

    evalInstruction l v (Acc n) = ProgrammeState (l+1) (Accumulator (v+n))
    evalInstruction l v (Jmp n) = ProgrammeState (l+n) (Accumulator v)
    evalInstruction l v (Nop _) = ProgrammeState (l+1) (Accumulator v)

    runLine :: [Instruction] -> ([Int], ProgrammeState) -> Maybe ([Int], ProgrammeState)
    runLine [] _ = Nothing
    runLine instructions (visitedLines, (ProgrammeState l (Accumulator v))) | l `elem` visitedLines = Nothing
                                                                            | otherwise = Just $ ( l: visitedLines, evalInstruction l v (instructions !! l))

    iterateWhileRunning :: (([Int], a) -> Maybe ([Int], a)) -> ([Int], a) -> [([Int], a)]
    iterateWhileRunning run state = case aDash of
                                      Just value -> value : (iterateWhileRunning run value)
                                      Nothing -> []
        where
          aDash = run state

    runInstructions instructions = iterateWhileRunning (runLine instructions) ([], (ProgrammeState 0 (Accumulator 0)))

    day08Pt1 = last . runInstructions <$> map (fromRight' . parse parseInstruction "") <$> readInstructions

    readInstructions :: IO [String]
    readInstructions = lines <$> readFile "resource/2020/day08"