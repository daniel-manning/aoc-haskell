module Day14_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List (foldl')
    import qualified Data.HashMap.Strict as H
    import Data.Universe.Helpers
    
    data Command = SetBitMask String | SetMemory Integer Integer deriving Show
    data MemorySystem = MemorySystem String (H.HashMap Integer Integer) deriving Show

    parseBitMask = do
        string "mask = "
        x <- many1 alphaNum
        return $ SetBitMask x

    parseMemory = do
        string "mem["
        n <- many1 digit
        string "] = "
        v <- many1 digit
        return $ SetMemory (read n) (read v)
    
    parseCommand :: Parser Command
    parseCommand = choice [try parseBitMask, try parseMemory]

    toBinary n = reverse $ toBits n
    toBits 0 = []
    toBits n = n `mod` 2 : toBits (n `div` 2)

    fromBinary :: [Integer] -> Integer
    fromBinary = foldl' (\a b -> 2*a + b) 0

    padToLength :: [Integer] -> [Integer]
    padToLength x = replicate (36 - length x) 0 ++ x

    bitMaskRule :: Char -> Integer -> Integer
    bitMaskRule 'X' n = n
    bitMaskRule '1' _ = 1
    bitMaskRule '0' _ = 0

    applyBitMask :: String -> Integer -> Integer
    applyBitMask bitMask n = fromBinary $ zipWith bitMaskRule bitMask (padToLength $ toBinary n)

    runCommand :: MemorySystem -> Command -> MemorySystem
    runCommand (MemorySystem bm map) (SetBitMask mask) = MemorySystem mask map
    runCommand (MemorySystem bm map) (SetMemory key value) = MemorySystem bm (H.insert key (applyBitMask bm value) (H.delete key map))


    sumMemoryValues :: MemorySystem -> Integer
    sumMemoryValues (MemorySystem _ m) = sum $ H.elems m

    day14Pt1 = sumMemoryValues . foldl' runCommand (MemorySystem "" H.empty) <$> parseCommands

    -------------
    memoryAddressDecoderRule :: Char -> Integer -> [Integer]
    memoryAddressDecoderRule 'X' n = [0,1]
    memoryAddressDecoderRule '1' _ = [1]
    memoryAddressDecoderRule '0' n = [n]

    applyMemoryAddressBitMask :: String -> Integer -> [Integer]
    applyMemoryAddressBitMask bitMask n = map fromBinary . choices $ zipWith memoryAddressDecoderRule bitMask (padToLength $ toBinary n)

    runMemoryAddressCommand :: MemorySystem -> Command -> MemorySystem
    runMemoryAddressCommand (MemorySystem bm map) (SetBitMask mask) = MemorySystem mask map
    runMemoryAddressCommand (MemorySystem bm map) (SetMemory key value) = MemorySystem bm (foldl' (\a b -> H.insert b value (H.delete b a)) map addresses)
        where
            addresses = applyMemoryAddressBitMask bm key

    day14Pt2 = sumMemoryValues . foldl' runMemoryAddressCommand (MemorySystem "" H.empty) <$> parseCommands

    -------------
    parseCommands = map (fromRight' . parse parseCommand "") <$> readCommands


    readCommands :: IO [String]
    readCommands = lines <$> readFile "resource/2020/day14"