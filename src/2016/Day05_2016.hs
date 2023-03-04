module Day05_2016 (
) where


import Data.Hash.MD5 (md5s, Str(..))
import Data.Maybe (isJust)
import Data.Char (digitToInt)
import Control.Monad (foldM)
import Data.List (sortOn, sort)
import Data.Either.Combinators (fromLeft')

findChar :: String -> Maybe Char
findChar s | take 5 hs == "00000" = Just (hs !! 5)
           | otherwise = Nothing
    where
        hs = md5s (Str s)

search :: String -> Maybe String
search prefix = sequence $ take 8 $ filter isJust $ map (\n -> findChar (prefix ++ show n)) [0..]

runPt1 = search "cxdnnyjw"
----------------
findChar' :: String -> Maybe (Int, Char)
findChar' s | take 5 hs == "00000" && p < 8 = Just ( p, hs !! 6)
            | otherwise = Nothing
    where
        hs = md5s (Str s)
        p = digitToInt (hs !! 5)


f :: [(Int, Char)] -> Maybe (Int, Char) -> Either [(Int, Char)] [(Int, Char)]
f cs (Just (n, c)) | sort (map fst cs) == [0..7] = Left cs
                   | n `elem` map fst cs = Right cs
                   | otherwise = Right ((n, c): cs) 


gatherAndOrder :: [Maybe (Int, Char)] -> String
gatherAndOrder = map snd . sortOn fst . fromLeft' . foldM f []


search' :: String -> String
search' prefix = gatherAndOrder $ filter isJust $ map (\n -> findChar' (prefix ++ show n)) [0..]

runPt2 = search' "cxdnnyjw"