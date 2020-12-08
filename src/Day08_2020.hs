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
                                                                            | l >= (length instructions) = Nothing
                                                                            | otherwise = Just $ ( l: visitedLines, evalInstruction l v (instructions !! l))

    iterateWhileRunning :: (([Int], a) -> Maybe ([Int], a)) -> ([Int], a) -> [([Int], a)]
    iterateWhileRunning run state = case aDash of
                                      Just value -> value : (iterateWhileRunning run value)
                                      Nothing -> []
        where
          aDash = run state

    runInstructions instructions = iterateWhileRunning (runLine instructions) ([], (ProgrammeState 0 (Accumulator 0)))

    day08Pt1 = last . runInstructions <$> map (fromRight' . parse parseInstruction "") <$> readInstructions

-----------------------
    runLine'' :: [Instruction] -> Maybe Int -> ([(Int, Instruction)], ProgrammeState) -> Maybe ([(Int, Instruction)], ProgrammeState)
    runLine'' [] _ _ = Nothing
    runLine'' instructions Nothing (visitedLines, (ProgrammeState l (Accumulator v))) | l `elem` (map fst visitedLines) = Nothing
                                                                                      | l >= (length instructions) = Nothing
                                                                                      | otherwise = Just $ ( (l, (instructions !! l)): visitedLines, evalInstruction l v (instructions !! l))
    runLine'' instructions (Just lR) (visitedLines, (ProgrammeState l (Accumulator v))) | l `elem` (map fst visitedLines) = Nothing
                                                                                              | l >= (length instructions) = Nothing
                                                                                              | l == lR = Just $ ( (l, (swapInstruction (instructions !! l))): visitedLines, evalInstruction l v (swapInstruction (instructions !! l)))
                                                                                              | otherwise = Just $ ( (l, (instructions !! l)): visitedLines, evalInstruction l v (instructions !! l))

    iterateWhileRunning'' :: (([(Int, Instruction)], a) -> Maybe ([(Int, Instruction)], a)) -> ([(Int, Instruction)], a) -> [([(Int, Instruction)], a)]
    iterateWhileRunning'' run state = case aDash of
                                          Just value -> value : (iterateWhileRunning'' run value)
                                          Nothing -> []
            where
              aDash = run state

    runInstructions'' replaceLine instructions = iterateWhileRunning'' (runLine'' instructions replaceLine) ([], (ProgrammeState 0 (Accumulator 0)))

    loopMembers :: [Instruction] -> [(Int, Instruction)]
    loopMembers instructions = fst . last $ runInstructions'' Nothing instructions

    day08Pt2 = (\instructions -> filter (\(ProgrammeState n _) -> n == (length instructions)) $ map (\n -> snd . last $ runInstructions'' (Just n) instructions) $ linesToChange $ (loopMembers instructions)) <$> map (fromRight' . parse parseInstruction "") <$> readInstructions

    linesToChange:: [(Int, Instruction)] -> [Int]
    linesToChange l = map fst $ filter (canSwitchToEscape l) l

    swapInstruction (Acc n) = Acc n
    swapInstruction (Jmp n) = Nop n
    swapInstruction (Nop n) = Jmp n

    canSwitchToEscape :: [(Int, Instruction)] -> (Int, Instruction) -> Bool
    canSwitchToEscape loopPaths (n, (Acc value)) = False
    canSwitchToEscape loopPaths (n, (Jmp value)) = not $ (n + 1) `elem` (map fst loopPaths)
    canSwitchToEscape loopPaths (n, (Nop value)) = not $ (n + value) `elem` (map fst loopPaths)



    readInstructions :: IO [String]
    readInstructions = lines <$> readFile "resource/2020/day08"