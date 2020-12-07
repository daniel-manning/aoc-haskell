module Day07_2015
    (
    ) where

    import Data.Bits
    import qualified Data.HashMap.Strict as H
    import Control.Applicative (liftA2)
    import Data.Maybe (fromJust)
    import Data.List (foldl', sort)
    import Data.Word
    import Data.Char (digitToInt)
    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators

    data Instruction = ASSIGN Int | AND String String | OR String String | NOT String | LSHIFT Int String | RSHIFT Int String deriving Show
    data Command = Command Instruction String deriving Show

    parseAssign = do
        n <- many1 digit
        return $ ASSIGN (read n)

    parseAnd = do
        idA <- many1 letter
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
        n <- digit
        return $ LSHIFT (digitToInt n) idA

    parseRShift = do
            idA <- many1 letter
            space
            string "RSHIFT"
            space
            n <- digit
            return $ RSHIFT (digitToInt n) idA

    parseCommand = do
        instruction <- choice [try parseAssign, try parseAnd, try parseOr, try parseNot, try parseLShift, try parseRShift]
        space
        string "->"
        space
        location <- many1 letter
        return $ Command instruction location

    runCommand :: H.HashMap String Int -> Command -> H.HashMap String Int
    runCommand values (Command (ASSIGN signal) location) = H.insert location signal values
    runCommand values (Command (OR idA idB) location) = H.insert location (fromJust $ liftA2 (.|.) (H.lookup idA values) (H.lookup idB values)) (H.delete location values)
    runCommand values (Command (AND idA idB) location) = H.insert location (fromJust $ liftA2 (.&.) (H.lookup idA values) (H.lookup idB values)) (H.delete location values)
    runCommand values (Command (LSHIFT n idA) location) = H.insert location (fromJust $ flip shiftL n <$> (H.lookup idA values)) (H.delete location values)
    runCommand values (Command (RSHIFT n idA) location) = H.insert location (fromJust $ flip shiftR n <$> (H.lookup idA values)) (H.delete location values)
    runCommand values (Command (NOT idA) location) = H.insert location (fromJust $ (\n -> (fromIntegral n) :: Int) . complement . (\n -> (fromIntegral n) :: Word16) <$> (H.lookup idA values)) (H.delete location values)


    runCircuit = sort . H.toList <$> foldl' (\b a -> runCommand b a) H.empty <$> map (fromRight' . (parse parseCommand "")) <$> readInstructions

    readInstructions :: IO [String]
    readInstructions = lines <$> readFile "resource/2015/day07"