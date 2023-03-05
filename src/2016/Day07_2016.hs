module Day07_2016 (
) where

import Data.List (tails, isSubsequenceOf, isInfixOf)


data IPV7 = IPV7 {
    octets :: [String], 
    hypernets :: [String]
} deriving (Show, Eq, Ord)

splitOutHypernet ss = (hn, r)
    where
        hn = takeWhile (/= ']') $  drop 1 ss
        r = drop 1 $ dropWhile (/= ']') $  drop 1 ss

splitOutOctet ss = (ot, r)
    where
        ot = takeWhile (/= '[') ss
        r =  dropWhile (/= '[') ss

parseIPV7 :: IPV7 -> String -> IPV7
parseIPV7 (IPV7 o h) [] =  IPV7 o h
parseIPV7 (IPV7 o h) ss | head ss == '[' = let k = splitOutHypernet ss in parseIPV7 (IPV7 o (fst k : h)) (snd k) 
                        | otherwise = let k = splitOutOctet ss in parseIPV7 (IPV7 (fst k : o) h) (snd k) 

readData :: IO [String]
readData = lines <$> readFile "resource/2016/day07"

readAndParse = map (parseIPV7 (IPV7 [] [])) <$> readData
--------------------
sliding :: Int -> [a] -> [[a]]
sliding n xs = filter (\k -> length k == n) $ map (take n) (tails xs)

abba :: String -> Bool
abba [a, b, c, d] = a == d && b == c && a /= b
abba _ = False


hasABBA :: String -> Bool
hasABBA = any abba . sliding 4


hasTLS :: IPV7 -> Bool
hasTLS (IPV7 o h) = any hasABBA o && not (any hasABBA h)


runPt1 = length . filter hasTLS <$> readAndParse
---------------
hasABA :: String -> Bool
hasABA [a,b,c] = a == c && a /= b

abas :: [String] -> [String]
abas = filter hasABA . concatMap (sliding 3)

invertABA :: String -> String
invertABA [a,b, _] = [b,a,b]

hasBAB :: String -> [String] -> Bool
hasBAB s = any (isInfixOf bab)
   where
       bab = invertABA s

hasSSL (IPV7 o h) = any (`hasBAB` h) $ abas o

runPt2 = length . filter hasSSL <$> readAndParse