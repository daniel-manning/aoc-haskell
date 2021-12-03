{-# LANGUAGE QuasiQuotes #-}

module Day08_2015
    (
        day08Part1,
        day08Part2
    ) where


    readStrings :: IO [String]
    readStrings = lines <$> readFile "resource/2015/day08"

    removeEscapes:: String -> String 
    removeEscapes [] = []
    removeEscapes ('\\': 'x':a:b:c) = 'a': removeEscapes c
    removeEscapes ('\\':a:b) = 'a' : removeEscapes b 
    removeEscapes (a:xs) = a : removeEscapes xs

    classifyStrings:: String -> (String, Int, Int)
    classifyStrings s = (s, length s, length (removeEscapes s) - 2)

    totalise :: (String, Int, Int) -> Int
    totalise (s, lc, le) = lc - le

    day08Part1 = sum . map (totalise . classifyStrings) <$> readStrings

    --------------------------

    escapeString :: String -> String 
    escapeString [] = []
    escapeString ('\\': s) = '\\':'\\':escapeString s
    escapeString ('"': s) = '\\':'"':escapeString s
    escapeString (s: xs) = s:escapeString xs

    classifyEncodedStrings:: String -> (String, Int, Int)
    classifyEncodedStrings s = (s, length s, length (escapeString s) + 2)
    
    totalEncoded :: (String, Int, Int) -> Int
    totalEncoded (s, lc, le) = le - lc

    day08Part2 = sum . map (totalEncoded . classifyEncodedStrings) <$> readStrings
    