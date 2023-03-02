module Day05_2017 (
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

readData :: IO [Int]
readData = map read . lines <$> readFile "resource/2017/day05"



runIndex :: (Int, Int, IntMap.IntMap Int) -> (Int, Int)
runIndex (idx, n, refs) | idx < 0 || idx >= length refs = (idx, n)
                        | otherwise = runIndex (idx + refs IntMap.! idx, n + 1, updateRefs)
    where
        updateRefs = IntMap.adjust (+1) idx refs

makeIndex refs = (0,  0, IntMap.fromList (zip [0..] refs))

runPt1 =  snd . runIndex . makeIndex <$> readData
---------------------

runIndex' :: (Int, Int, IntMap.IntMap Int) -> (Int, Int)
runIndex' (idx, n, refs) | idx < 0 || idx >= length refs = (idx, n)
                        | otherwise = runIndex' (idx + refs IntMap.! idx, n + 1, updateRefs)
    where
        updateRefs | refs IntMap.! idx >= 3 = IntMap.adjust (\n -> n-1) idx refs
                   |otherwise = IntMap.adjust (+1) idx refs

runPt2 =  snd . runIndex' . makeIndex <$> readData