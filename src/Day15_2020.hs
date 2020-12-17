module Day15_2020
    (
    day15Pt2
    ) where

    import Data.List (elemIndex, maximumBy)
    import Data.Maybe (fromMaybe)
    import qualified Data.HashMap.Strict as H

    considerLastNumberCondensed :: ((Int, Int), H.HashMap Int Int) -> ((Int, Int), H.HashMap Int Int)
    considerLastNumberCondensed ((i, lastNumber), numberMap) = ((i+1, maybe 0 (i -) foundIndex), H.insert lastNumber i (H.delete lastNumber numberMap))
        where
          foundIndex = H.lookup lastNumber numberMap

    times :: Int -> (a -> a) -> a -> a
    times 0 _ k = k
    times n f k = times (n-1) f $! f k

    --input = [0,3,6]
    nthNumberSpoken n input = snd $ fst $ times (n - length input) considerLastNumberCondensed ((snd h, fst h), updated)
        where
          inputFormat = zip input [1..]
          h = maximumBy (\a b -> compare (snd a) (snd b)) inputFormat
          updated = H.delete (fst h) (H.fromList inputFormat)

    day15Pt1 = nthNumberSpoken 2020 [7,12,1,0,16,2]
    --day15Pt2 = nthNumberSpoken 30000000 [0,3,6]
    day15Pt2 = nthNumberSpoken 15000000 [0,3,6]