module Day15_2020
    (
      day15Pt1,
      day15Pt2
    ) where

    import Data.List (elemIndex, maximumBy)
    import Data.Maybe (fromMaybe)
    import qualified Data.HashMap.Strict as H
    --import qualified Data.IntMap.Strict as M

    considerLastNumberCondensed :: ((Integer, Integer), H.HashMap Integer Integer) -> ((Integer, Integer), H.HashMap Integer Integer)
    considerLastNumberCondensed ((i, lastNumber), numberMap) = ((i+1, maybe 0 (i -) foundIndex), H.insert lastNumber i numberMap)
        where
          foundIndex = H.lookup lastNumber numberMap

    nthNumberSpoken :: Integer -> [Integer] -> Integer
    nthNumberSpoken n input = nthNumberSpoken'' n ((snd h, fst h), updated)
        where
          inputFormat = zip input [1..]
          h = maximumBy (\a b -> compare (snd a) (snd b)) inputFormat
          updated = H.delete (fst h) (H.fromList inputFormat)
        
    nthNumberSpoken'' :: Integer -> ((Integer, Integer), H.HashMap Integer Integer) -> Integer
    nthNumberSpoken'' n ((a,b), map) | a == n = b  
                                     | otherwise = nthNumberSpoken'' n  $! considerLastNumberCondensed ((a,b), map)       
          

    day15Pt1 = nthNumberSpoken 2020 [7,12,1,0,16,2]
    day15Pt2 = nthNumberSpoken 30000000 [7,12,1,0,16,2]

    {--
        238

        real	1m7.123s
        user	2m23.730s
        sys	1m11.120s
    --}