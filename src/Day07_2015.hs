module Day07_2015
    (
    ) where

    import Data.Bits
    import qualified Data.HashMap.Strict as H
    import Control.Applicative (liftA2)
    import Data.Maybe (fromJust, isJust)
    import Data.List (foldl', sort, find)
    import Data.Word
    import Data.Char (digitToInt)
    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators

    data Input = Node String | Signal Int deriving Show

    data Instruction = ASSIGN Input | AND Input String | OR String String | NOT String | LSHIFT Int String | RSHIFT Int String deriving Show
    data Command = Command Instruction String deriving Show

    parseAssign = do
        n <- choice [try parseSignal, try parseNode]
        return $ ASSIGN n

    parseNode = do
        idA <- many1 letter
        return $ Node idA

    parseSignal = do
        idA <- many1 digit
        return $ Signal (read idA)

    parseAnd = do
        idA <- choice [try parseSignal, try parseNode]
        space
        string "AND"
        space
        idB <- many1 letter
        return $ AND idA idB

    parseOr = do
        idA <- many1 letter
        space
        string "OR"
        space
        idB <- many1 letter
        return $ OR idA idB

    parseNot = do
        string "NOT"
        space
        idA <- many1 letter
        return $ NOT idA

    parseLShift = do
        idA <- many1 letter
        space
        string "LSHIFT"
        space
        n <- many1 digit
        return $ LSHIFT (read n) idA

    parseRShift = do
            idA <- many1 letter
            space
            string "RSHIFT"
            space
            n <- many1 digit
            return $ RSHIFT (read n) idA

    parseCommand = do
        instruction <- choice [try parseAnd, try parseOr, try parseNot, try parseLShift, try parseRShift, try parseAssign]
        space
        string "->"
        space
        location <- many1 letter
        return $ Command instruction location

    getValue :: Input -> H.HashMap String Int -> Maybe Int
    getValue (Signal value) values = Just value
    getValue (Node name) values = H.lookup name values

    runCommand :: H.HashMap String Int -> Command -> H.HashMap String Int
    runCommand values (Command (ASSIGN input) location) = H.insert location (fromJust $ getValue input values) values
    runCommand values (Command (OR idA idB) location) = H.insert location (fromJust $ liftA2 (.|.) (H.lookup idA values) (H.lookup idB values)) (H.delete location values)
    runCommand values (Command (AND input idB) location) = H.insert location (fromJust $ liftA2 (.&.) (getValue input values) (H.lookup idB values)) (H.delete location values)
    runCommand values (Command (LSHIFT n idA) location) = H.insert location (fromJust $ flip shiftL n <$> H.lookup idA values) (H.delete location values)
    runCommand values (Command (RSHIFT n idA) location) = H.insert location (fromJust $ flip shiftR n <$> H.lookup idA values) (H.delete location values)
    runCommand values (Command (NOT idA) location) = H.insert location (fromJust $ (\n -> fromIntegral n :: Int) . complement . (\n -> fromIntegral n :: Word16) <$> H.lookup idA values) (H.delete location values)

    runCommandIfReady :: H.HashMap String Int -> [Command] -> [Command] -> H.HashMap String Int
    runCommandIfReady values [] [] = values
    runCommandIfReady values notReady [] = runCommandIfReady values [] notReady
    runCommandIfReady values notReady ((Command (ASSIGN input) location): cs) = if isJust (getValue input values) then runCommandIfReady (runCommand values (Command (ASSIGN input) location)) notReady cs else  runCommandIfReady values (Command (ASSIGN input) location: notReady) cs
    runCommandIfReady values notReady ((Command (OR idA idB) location):cs) = if isJust (H.lookup idA values) && isJust (H.lookup idB values) then runCommandIfReady (runCommand values (Command (OR idA idB) location)) notReady cs else  runCommandIfReady values (Command (OR idA idB) location: notReady) cs
    runCommandIfReady values notReady ((Command (AND input idB) location):cs) = if isJust (getValue input values) && isJust (H.lookup idB values) then runCommandIfReady (runCommand values (Command (AND input idB) location)) notReady cs else  runCommandIfReady values (Command (AND input idB) location: notReady) cs
    runCommandIfReady values notReady ((Command (LSHIFT n idA) location):cs) = if isJust (H.lookup idA values) then runCommandIfReady (runCommand values (Command (LSHIFT n idA) location)) notReady cs else  runCommandIfReady values (Command (LSHIFT n idA) location: notReady) cs
    runCommandIfReady values notReady ((Command (RSHIFT n idA) location):cs) = if isJust (H.lookup idA values) then runCommandIfReady (runCommand values (Command (RSHIFT n idA) location)) notReady cs else  runCommandIfReady values (Command (RSHIFT n idA) location: notReady) cs
    runCommandIfReady values notReady ((Command (NOT idA) location):cs) = if isJust (H.lookup idA values) then runCommandIfReady (runCommand values (Command (NOT idA) location)) notReady cs else  runCommandIfReady values (Command (NOT idA) location: notReady) cs

    runCircuit = (sort . H.toList <$> foldl' runCommand H.empty) . map (fromRight' . parse parseCommand "") <$> readInstructions
    runCircuitWaitingForInputs = (sort . H.toList <$> runCommandIfReady H.empty []) . map (fromRight' . parse parseCommand "") <$> readInstructions
    runCircuitWaitingForInputsPt2 = (sort . H.toList <$> runCommandIfReady H.empty []) . map (fromRight' . parse parseCommand "") <$> readInstructionsPt2

    day07Pt1 = snd . fromJust . find (\l -> fst l == "a") <$> runCircuitWaitingForInputs
    day07Pt2 = snd . fromJust . find (\l -> fst l == "a") <$> runCircuitWaitingForInputsPt2

    readInstructions :: IO [String]
    readInstructions = lines <$> readFile "resource/2015/day07"

    readInstructionsPt2 :: IO [String]
    readInstructionsPt2 = lines <$> readFile "resource/2015/day07_pt2"