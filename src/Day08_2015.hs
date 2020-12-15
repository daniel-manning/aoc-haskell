module Day08_2015
    (
    ) where

    readStrings :: IO [String]
    readStrings = lines <$> readFile "resource/2015/day08_test"