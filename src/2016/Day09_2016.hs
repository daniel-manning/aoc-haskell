module Day09_2016 (
    day09Pt2
) where

import Data.Bifunctor (bimap)


readData :: IO [String]
readData = lines <$> readFile "resource/2016/day09"
----------------------

data Marker = Marker Int Int deriving Show

createMarker :: String -> Marker
createMarker = uncurry Marker . bimap read (read . drop 1) . span (/= 'x')

extractMarker :: String -> (Marker, String)
extractMarker = bimap createMarker (drop 1) . span (/= ')')

repeatMarker :: Marker -> String -> (String, String)
repeatMarker (Marker l n) xs = (concat $ replicate n (take l xs), drop l xs)

findMarkerAndRepeat :: String -> String
findMarkerAndRepeat [] = []
findMarkerAndRepeat ('(' : xs) = repeats ++ findMarkerAndRepeat xs''
    where
        (m, xs') = extractMarker xs
        (repeats, xs'') = repeatMarker m xs' 
findMarkerAndRepeat (x: xs) = x : findMarkerAndRepeat xs

lengthOfDecompressedString :: String -> Int
lengthOfDecompressedString = length . findMarkerAndRepeat

day09Pt1 = head . map lengthOfDecompressedString <$> readData
----------------------------

repeatMarker' :: Marker -> String -> (Int, String)
repeatMarker' (Marker l n) xs | '(' `elem` k = ( n * findMarkerAndRepeat' k , drop l xs)
                              | otherwise = (n* length k, drop l xs)
    where
        k = take l xs

findMarkerAndRepeat' :: String -> Int
findMarkerAndRepeat' [] = 0
findMarkerAndRepeat' ('(' : xs) = repeats + findMarkerAndRepeat' xs''
    where
        (m, xs') = extractMarker xs
        (repeats, xs'') = repeatMarker' m xs' 
findMarkerAndRepeat' (x: xs) = 1 + findMarkerAndRepeat' xs

lengthOfDecompressedString' :: String -> Int
lengthOfDecompressedString' = findMarkerAndRepeat'

day09Pt2 = head . map lengthOfDecompressedString' <$> readData