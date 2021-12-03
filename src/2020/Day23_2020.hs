module Day23_2020
    (
        day23Pt2
    ) where

    import Data.Set (Set)
    import qualified Data.Set as Set
    import Data.Maybe (fromMaybe, fromJust)
    import Data.List (find)
   
    findDestination :: Integer -> [Integer] -> Integer
    findDestination x xs =  fromMaybe (maximum xs) $ Set.lookupMax $ Set.intersection (Set.fromList xs) (Set.fromList [0..x])

    insertListAtDestination :: [Integer] -> Integer -> [Integer] -> [Integer]
    insertListAtDestination ls d xs = (\n -> (take (n+1) xs) ++ ls ++ (drop (n+1) xs)) $ snd $ fromJust $ find (\(a,b) -> a == d) $ zip xs [0..]

    runCups :: [Integer] -> [Integer]
    runCups (x:a:b:c:xs) = (insertListAtDestination [a,b,c] (findDestination x xs) xs) ++ [x]

    makeNMoves :: Integer -> [Integer] -> [Integer]
    makeNMoves 0 xs = xs
    makeNMoves n xs = makeNMoves (n-1) $! runCups xs

    remakeLabelling :: [Integer] -> String
    remakeLabelling xs = show =<< ((\n -> (drop (n+1) xs) ++ (take n xs)) $ snd $ fromJust $ find (\(a,b) -> a == 1) $ zip xs [0..])

    findTwoCupsAfterOne :: [Integer] -> [Integer]
    findTwoCupsAfterOne = take 2 . tail . dropWhile (/= 1)

    test = [3,8,9,1,2,5,4,6,7]
    day23 = [9,5,2,4,3,8,7,1,6]

    day23Pt1 = remakeLabelling $ makeNMoves 100 day23
    day23Pt2 =  findTwoCupsAfterOne $ makeNMoves 10000000 (test ++ [10..1000000])
